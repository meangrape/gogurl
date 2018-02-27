{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Control.Applicative
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Trans
import Data.Aeson hiding (json)
import Data.IORef
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.Persist hiding (get)
import Database.Persist.Sqlite hiding (get)
import Database.Persist.TH

data MySession =
  EmptySession

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Link json
  name       T.Text
  url        T.Text
  hits       Int default=0
  created_at UTCTime default=CURRENT_TIMESTAMP
  UniqueName name
  UniqueURL  url
  deriving Show
|]

type Api = SpockM SqlBackend MySession () ()

type ApiAction a = SpockAction SqlBackend MySession () a

runSQL ::
     (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a
  -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: Int -> T.Text -> ApiAction ()
errorJson code message =
  json $
  object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "db/links.db" 5
  spockCfg <- defaultSpockCfg EmptySession (PCPool pool) ()
  runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
  runSpock 80 (spock spockCfg app)

app :: Api
app = do
  get root $ do
    topLinks <- runSQL $ selectList [] [Desc LinkHits]
    json $ object ["result" .= String "success", "links" .= topLinks]
  get "links" $ redirect "/"
  post "links" $ do
    maybeLink <- jsonBody' :: ApiAction (Maybe Link)
    case maybeLink of
      Nothing -> errorJson 1 "Failed to parse request body as Link"
      Just theLink -> do
        newID <- runSQL $ insert theLink
        json $ object ["result" .= String "success", "id" .= newID]
  get ("links" <//> var <//> "delete") $ \linkName -> do
    runSQL $ deleteWhere [LinkName ==. linkName]
    json $ object ["result" .= String "success", "name" .= linkName]
  -- The 2d value must be a URL
  get ("links" <//> var <//> "edit" <//> var) $ \linkName linkURL -> do
    runSQL $ updateWhere [LinkName ==. linkName] [LinkUrl =. linkURL]
    json $ object ["result" .= String "success", "id" .= linkName]
  get var $ \linkName -> do
    maybeLink <- runSQL $ selectFirst [LinkName ==. linkName] []
    case maybeLink of
      Nothing -> errorJson 2 "No record found"
      Just theLink -> do
        runSQL $ updateWhere [LinkName ==. linkName] [LinkHits +=. 1]
        let theURL = linkUrl $ entityVal theLink
        redirect theURL
