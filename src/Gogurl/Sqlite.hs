{-# language LambdaCase #-}
{-# language TemplateHaskell #-}

-- | This module is intented to replace @sqlite-simple@. It provides a
-- quasi-quoter with variable interpolation as a lightweight alternative to
-- the "named" query support provided by @sqlite-simple@.

module Gogurl.Sqlite
  ( Query
  , sql
  , execute
  , executeFile
  , query
    -- * Re-exports
  , module X
  ) where

import Control.Monad.Fail (MonadFail)
import Data.Char (isAlphaNum, isLower, isSpace)
import Data.Text (pack)
import Database.SQLite.Simple (Connection, FromRow, NamedParam)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (lift)

import qualified Data.Text.IO as Text
import qualified Database.SQLite.Simple as Sqlite

import Database.SQLite.Simple as X
  (Connection, FromRow, Only(Only), close, lastInsertRowId, open)

data Query
  = Query [Char] [NamedParam]

execute :: Connection -> Query -> IO ()
execute conn (Query q params) =
  Sqlite.executeNamed conn (Sqlite.Query (pack q)) params

executeFile :: Connection -> FilePath -> IO ()
executeFile conn path = do
  bytes <- Text.readFile path
  Sqlite.execute_ conn (Sqlite.Query bytes)

query :: FromRow r => Connection -> Query -> IO [r]
query conn (Query q params) =
  Sqlite.queryNamed conn (Sqlite.Query (pack q)) params

sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quoteExp = \s -> do
        let s' = unwords (map (dropWhile isSpace) (lines s))
        names <- parseParams s'
        conE 'Query `appE` lift s' `appE` listE (map namedParam names)

    , quoteDec = error "sql: quoteDec not defined"
    , quotePat = error "sql: quotePat not defined"
    , quoteType = error "sql: quoteType not defined"
    }

-- | Pluck out the params prefixed by ':' from a string.
--
-- >>> parseParams "foo :bar baz :qux"
-- [["bar", "qux"]]
parseParams :: MonadFail m => [Char] -> m [[Char]]
parseParams = \case
  [] ->
    pure []
  ':':x:xs
    | isIdent1 x ->
        let
          (ys, zs) =
            span isIdent xs
        in
          ((x:ys):) <$> parseParams zs
    | otherwise ->
        fail ("Invalid identifier: " ++ x:xs)
  _:xs ->
    parseParams (dropWhile (/= ':') xs)
 where
  isIdent1 :: Char -> Bool
  isIdent1 x =
    isLower x || x == '_'

  isIdent :: Char -> Bool
  isIdent x =
    isAlphaNum x || x == '_' || x == '\''

-- | Make a Template Haskell expression of a NamedParam.
--
-- For example,
--
--   namedParam "foo"
--
-- would result in the following expression:
--
--   ":foo" := foo
namedParam :: [Char] -> Q Exp
namedParam s =
  lookupValueName s >>= \case
    Nothing ->
        fail ("Variable " ++ s ++ " not in scope")
    Just name ->
      conE '(Sqlite.:=) `appE` lift (':' : s) `appE` varE name
