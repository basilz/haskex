{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ch4.CsvChallenge where

import qualified Control.Foldl as Foldl
import Control.Monad
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Text
import Data.Time
import Data.Vector
import GHC.Generics

data Gender = Male | Female deriving (Eq, Generic, Show)

data Person = Person {name :: !Text, gender :: !Gender, dob :: !Day}
  deriving (Generic, Show)

data Stats = Stats {femaleNum :: Int, maleNum :: Int, oldestPerson :: Maybe Person}
  deriving (Show)

instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . B8.unpack

instance FromField Gender where
  parseField s = case toLower (strip (pack (B8.unpack s))) of
    "male" -> pure Male
    "female" -> pure Female
    _ -> mzero

instance FromRecord Person where
  parseRecord v = Person <$> v .! 0 <*> v .! 1 <*> v .! 2

readCsvFile :: FilePath -> IO (Vector Person)
readCsvFile fp = do
  csvData <- BL.readFile fp
  case decode NoHeader csvData of
    Left err -> error err
    Right pp -> return pp

stats :: Foldl.Fold Person Stats
stats = Foldl.Fold step begin done
  where
    begin = (0, 0, Nothing)
    step (_, _, Nothing) p@(Person _ Female _) = (1, 0, Just p)
    step (_, _, Nothing) p@(Person _ Male _) = (0, 1, Just p)
    step (f, m, Just o) p@(Person _ Female _) = (f + 1, m, oldestByDob o p)
    step (f, m, Just o) p@(Person _ Male _) = (f, m + 1, oldestByDob o p)
    done (f, m, db) = Stats f m db
    oldestByDob p1 p2 = if dob p1 < dob p2 then Just p1 else Just p2

main :: FilePath -> IO ()
main fp = readCsvFile fp >>= print . Foldl.fold stats
