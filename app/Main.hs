{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( FromJSON, Value(String)
  , (.:), (.=)
  , object, parseJSON, withObject
  )
import Data.Int (Int64)
import Database.SQLite.Simple
  ( Connection
  , FromRow
  , Only(Only, fromOnly)
  , Query(Query)
  , ToRow
  )
import Data.Pool
import qualified Database.SQLite.Simple as Sqlite
import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)
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
      main'
        <$> option auto
              (mconcat
                [ short 'p'
                , long "port"
                , help "port"
                , metavar "PORT"
                , value 8082
                , showDefault
                ])
        <*> strOption
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
  createTables <- T.readFile "db/links.sql"
  withConn (\conn -> Sqlite.execute conn (Query createTables) ())

  -- Run the web server.
  scotty port (app withConn)

app :: (forall r. (Connection -> IO r) -> IO r) -> ScottyM ()
app withConn = do
  -- Partially apply Sqlite functions to the given database runner, so the
  -- handlers become more readable.

  let -- Execute a SQL statement.
      statement :: ToRow v => Query -> v -> ActionM ()
      statement q v = liftIO (withConn (\conn -> Sqlite.execute conn q v))

  let -- Execute a SQL query.
      query :: (FromRow r, ToRow v) => Query -> v -> ActionM [r]
      query q v = liftIO (withConn (\conn -> Sqlite.query conn q v))

  let -- Get the rowid of the last successful INSERT statement.
      lastInsertRowId :: ActionM Int64
      lastInsertRowId = liftIO (withConn Sqlite.lastInsertRowId)

  -- Here are the actual handlers.

  get "/" $ do
    topUrls <- query "SELECT url FROM Link ORDER BY hits DESC" ()
    json (map fromOnly topUrls :: [Text])

  get "/links" $ do
    redirect "https://go/"

  post "/links" $ do
    Link name url <- jsonData
    statement "INSERT INTO link (name, url) VALUES (?, ?)" (name, url)
    newID <- lastInsertRowId
    json $ object ["result" .= String "success", "id" .= newID]

  get "/links/:name/delete" $ do
    name <- param "name"
    statement "DELETE FROM link WHERE name = ?" (Only (name :: Text))
    json $ object ["result" .= String "success", "name" .= name]

  get "/links/:name/edit/:url" $ do
    name :: Text <- param "name"
    url :: Text <- param "url"
    statement "UPDATE link SET url = ? WHERE name = ?" (url, name)
    json $ object ["result" .= String "success", "id" .= name]

  get "/:name" $ do
    name :: Text <- param "name"
    urls <- query "SELECT url FROM link WHERE name = ?" (Only name)
    case urls of
      [] -> errorJson 2 "No record found"
      Only url:_ -> do
        statement "UPDATE link SET hits = hits + 1 WHERE name = ?" (Only name)
        redirect url
