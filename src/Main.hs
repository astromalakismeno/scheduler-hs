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
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Time.Implementation 

import Data.Time.Clock
import Data.Time.LocalTime (TimeOfDay)

data Person = Person { idPerson :: Int, surname :: String, first :: String }
data Shift = Shift { name :: String, start :: String, end :: String}
data Counter = Counter { idCounter :: Int, personId :: Int, shiftId :: String, value :: Int }

instance Show Person where
    show (Person i s _) = (show i) ++ "\t" ++ s

instance Show Shift where
    show (Shift n s e) = n ++ "\t" ++ (show (read s :: TimeOfDay)) ++ ":" ++ (show (read s :: TimeOfDay))

instance Show Counter where
    show (Counter c p s v) = show c ++ show p ++ show s ++ show v

instance FromRow Person where
    fromRow = Person <$> field <*> field <*> field

instance FromRow Shift where
    fromRow = Shift <$> field <*> field <*> field

instance FromRow Counter where
    fromRow = Counter <$> field <*> field <*> field <*> field

insertPerson conn id surname first = execute conn "insert into person (id,surname,first) values (?,?,?)"
							(id,surname, first)

insertShift :: Connection -> String -> String -> String -> IO ()
insertShift conn name start end =
    execute conn "insert into shift (name,start,end) values (?,?,?)" (name,(show (read start::TimeOfDay)),(show (read end::TimeOfDay)))

main :: IO ()
main = do
  conn <- open "dist/resources/test.db"
  p <- query_ conn "select * from person" :: IO [Person]
  s <- query_ conn "select * from shift" :: IO [Shift]
  c <- query_ conn "select * from counter" :: IO [Counter]
  mapM_ print p
  mapM_ print s
  mapM_ print c
  close conn

