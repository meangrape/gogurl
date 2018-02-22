{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Control.Applicative
import Control.Monad.Trans
import Data.DateTime
import Data.Monoid
import Data.IORef
import qualified Data.Text as T
import Data.Time.Clock    (UTCTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow


data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)
data Link = Link
  { name       :: T.Text
  , url        :: T.Text
  , hits       :: Int
  , created_at :: UTCTime
  }


main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app =
    do get root $
           text "Show links"
       get ("hello" <//> var) $ \name ->
           do (DummyAppState ref) <- getState
              visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
              text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
       get "links"
            text "Redirect to root"
       post "links"
            text "Create a link"
       get "links/search"
            text "Here's a link"
       get ("links" <//> id <//> "delete") $ \id ->
            text "Delete a link"
       get ("links" <//> id <//> "edit") $ \id ->
            text "Edit a link"
       get (name <//> ABC)
            text "The real deal"

