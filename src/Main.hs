{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  BSD
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless)
import Data.List (stripPrefix)
import System.Exit (exitFailure)

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import Data.Time.Clock

data Person = Person { idPerson :: Id, surname :: String, firstname :: String }
data Shift = Shift { idShift :: Id, name :: String, period :: Period }
data Counter = Counter { idCounter :: Id, personId :: Id, shiftId :: Id, value :: Int }
data Period = Period UTCTime UTCTime

type Counters = [Counter]
type Id = Int

instance Show Person where
    show (Person i s _) = (show i) ++ "\t" ++ s

instance Show Counter where
    show (Counter c p s v) = show c ++ show p ++ show s ++ show v

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field

instance FromRow Counter where
  fromRow = Counter <$> field <*> field <*> field <*> field

main :: IO ()
main = do
  conn <- open "dist/resources/test.db"
  r <- query_ conn "select * from person" :: IO [Person]
  --r <- query_ conn "select * from shift" :: IO [Shift]
  r <- query_ conn "select * from counter" :: IO [Counter]
  mapM_ print r
  close conn

