{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Gogurl.Sqlite (Connection, FromRow, Only(Only), Query, sql)

import qualified Gogurl.Sqlite as Sqlite
  ( close
  , execute
  , executeFile
  , lastInsertRowId
  , open
  , query
  )

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( FromJSON
  , Value(String)
  , (.:)
  , (.=)
  , object
  , parseJSON
  , withObject
  )
import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.Pool
import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy as Lazy
import Options.Applicative
import Web.Scotty

data Link = Link
  { linkName :: Text
  , linkUrl :: Text
  }

instance FromJSON Link where
  parseJSON = withObject "link" (\o -> Link <$> o .: "name" <*> o .: "url")

errorJson :: Int -> Text -> ActionM ()
errorJson code message =
  json $
  object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

main :: IO ()
main = join (execParser (info (helper <*> parser) mempty))
  where
    parser =
      main' <$>
      option
        auto
        (mconcat
           [ short 'p'
           , long "port"
           , help "port"
           , metavar "PORT"
           , value 8082
           , showDefault
           ]) <*>
      strOption
        (mconcat
           [ short 'd'
           , long "database"
           , help "Sqlite database connection string"
           , metavar "DATABASE"
           , value "db/links.db"
           , showDefault
           ])

main' :: Int -> String -> IO ()
main' port database = do
  pool <- createPool (Sqlite.open database) Sqlite.close 1 5 5
  let withConn :: (Connection -> IO r) -> IO r
      withConn = withResource pool
  -- Create tables if they don't exist.
  withConn (\conn -> Sqlite.executeFile conn "db/links.sql")
  -- Run the web server.
  scotty port (app withConn)

styledPage :: Lazy.Text -> Lazy.Text
styledPage body =
  "<!DOCTYPE HTML><html><head><title>gogurl</title><style>" <>
  "body { font-family: sans-serif; color: #444; width: 480px; margin: 0 auto; }" <>
  "ul { padding: 0; list-style: none; }" <>
  "h1 { margin: 48px 0 24px; }" <>
  ".prefix { color: #999 }" <>
  "a { text-decoration: none; color: inherit; }" <>
  "li { margin: 16px 0; transition: transform ease-out 120ms }" <>
  "li:hover { transform: translateX(8px) } li p { margin: 8px 0; }" <>
  "</style></head><body><h1>gogurl</h1>" <>
  body <>
  "</body></html>"

app :: (forall r. (Connection -> IO r) -> IO r) -> ScottyM ()
app withConn
  -- Partially apply Sqlite functions to the given database runner, so the
  -- handlers become more readable.
      -- Execute a SQL statement.
 = do
  let statement :: Query -> ActionM ()
      statement q = liftIO (withConn (\conn -> Sqlite.execute conn q))
      -- Execute a SQL query.
  let query :: FromRow r => Query -> ActionM [r]
      query q = liftIO (withConn (\conn -> Sqlite.query conn q))
      -- Get the rowid of the last successful INSERT statement.
  let lastInsertRowId :: ActionM Int64
      lastInsertRowId = liftIO (withConn Sqlite.lastInsertRowId)
  -- Here are the actual handlers.
  get "/" $ do
    topUrls :: [(Text, Text)] <-
      query [sql| SELECT name, url FROM Link ORDER BY hits DESC|]
    html $
      styledPage $
      "<ul>" <>
      (mconcat $
       map
         (\(a, b) ->
            fromStrict $
            mconcat
              [ "<li><a href='/"
              , a
              , "'><p><span class='prefix'>go/</span>"
              , a
              , "</p><p>"
              , b
              , "</p>"
              , "</a></li>"
              ])
         topUrls) <>
      "</ul>"
  get "/links" $ redirect "http://go/"
  post "/links" $ do
    Link name url <- jsonData
    statement
      [sql| INSERT INTO link (name, url)
                    VALUES (:name, :url) |]
    newID <- lastInsertRowId
    json $ object ["result" .= String "success", "id" .= newID]
  get "/links/:name/delete" $ do
    name :: Text <- param "name"
    statement
      [sql| DELETE FROM link
                    WHERE name = :name |]
    json $ object ["result" .= String "success", "name" .= name]
  get "/links/:name/edit/:url" $ do
    name :: Text <- param "name"
    url :: Text <- param "url"
    statement
      [sql| UPDATE link
                    SET url = :url
                    WHERE name = :name |]
    json $ object ["result" .= String "success", "id" .= name]
  get "/:name" $ do
    name :: Text <- param "name"
    urls <-
      query
        [sql| SELECT url
                        FROM link
                        WHERE name = :name |]
    case urls of
      [] -> errorJson 2 "No record found"
      Only url:_ -> do
        statement
          [sql| UPDATE link
                        SET hits = hits + 1
                        WHERE name = :name |]
        redirect url
