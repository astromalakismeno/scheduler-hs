{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
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
import Test.QuickCheck.All (quickCheckAll)

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import Data.Time.Clock

data Person = Person Id Surname Firstname
data Shift = Shift Id Name Period
data Counter = Counter Id PersonId ShiftId Int
data Period = Period UTCTime UTCTime
type Counters = [Counter]
type Id = Int
type PersonId = Int
type Name = String
type Surname = String
type Firstname = String

instance Show Person where
    show (Person i s _) = (show i) ++ "\t" ++ s

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field

instance FromRow Counter where
  fromRow = Counter <$> field <*> field <*> field <*> field

main :: IO ()
main = do
  conn <- open "dist/resources/test.db"
 -- execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 2" :: String))
  r <- query_ conn "SELECT * from person" :: IO [Person]
  mapM_ print r
  close conn

