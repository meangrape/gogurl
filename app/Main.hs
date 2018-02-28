{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Control.Applicative
import Data.Aeson hiding (json)
import Data.Char (toLower)
import Data.IORef
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.SQLite.Simple
  ( FromRow
  , Only(Only, fromOnly)
  , Query(Query)
  , ToRow
  )
import qualified Database.SQLite.Simple as Sqlite

data MySession =
  EmptySession

data Link = Link
  { linkName :: Text
  , linkUrl :: Text
  }

instance FromJSON Link where
  parseJSON = withObject "link" (\o -> Link <$> o .: "name" <*> o .: "url")

type Api = SpockM Sqlite.Connection MySession () ()

type ApiAction a = SpockAction Sqlite.Connection MySession () a

sqlQuery :: (FromRow r, ToRow s) => Query -> s -> ApiAction [r]
sqlQuery query row = runQuery (\conn -> Sqlite.query conn query row)

sqlQuery_ :: FromRow r => Query -> ApiAction [r]
sqlQuery_ query = runQuery (\conn -> Sqlite.query_ conn query)

sqlStmt ::
     (HasSpock m, SpockConn m ~ Sqlite.Connection, ToRow r)
  => Query
  -> r
  -> m ()
sqlStmt query row = runQuery (\conn -> Sqlite.execute conn query row)

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
  object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg EmptySession (PCConn connBuilder) ()
  createTables <- T.readFile "db/links.sql"
  runSpock 8082 $
    spock spockCfg $ do
      sqlStmt (Query createTables) ()
      app
  where
    connBuilder :: ConnBuilder Sqlite.Connection
    connBuilder =
      ConnBuilder
      { cb_createConn = Sqlite.open "db/links.db"
      , cb_destroyConn = Sqlite.close
      , cb_poolConfiguration =
          PoolCfg
          { pc_stripes = 1 -- Number of independently managed database pools
          , pc_resPerStripe = 5 -- Number of connections per pool
          , pc_keepOpenTime = 5 -- Keepalive, in seconds
          }
      }

app :: Api
app = do
  get root $ do
    topUrls <- sqlQuery "SELECT url FROM Link ORDER BY hits DESC" ()
    html $ T.pack $ show (map fromOnly topUrls :: [Text])
  get "links" $ redirect "https://go/"
  post "links" $ do
    maybeLink <- jsonBody' :: ApiAction (Maybe Link)
    case maybeLink of
      Nothing -> errorJson 1 "Failed to parse request body as Link"
      Just Link {linkName, linkUrl} -> do
        sqlStmt "INSERT INTO link (name, url) VALUES (?, ?)" (linkName, linkUrl)
        newID <- runQuery Sqlite.lastInsertRowId
        json $ object ["result" .= String "success", "id" .= newID]
  get ("links" <//> var <//> "delete") $ \linkName -> do
    sqlStmt "DELETE FROM link WHERE name = ?" (Only (linkName :: Text))
    json $ object ["result" .= String "success", "name" .= linkName]
  -- The 2d value must be a URL
  get ("links" <//> var <//> "edit" <//> var) $ \linkName linkUrl -> do
    sqlStmt
      "UPDATE link SET url = ? WHERE name = ?"
      (linkUrl :: Text, linkName :: Text)
    json $ object ["result" .= String "success", "id" .= linkName]
  get var $ \linkName -> do
    linkUrls <-
      sqlQuery "SELECT url FROM link WHERE name = ?" (Only (linkName :: Text))
    case linkUrls of
      [] -> errorJson 2 "No record found"
      Only linkUrl:_ -> do
        sqlStmt "UPDATE link SET hits = hits + 1 WHERE name = ?" (Only linkName)
        redirect linkUrl
