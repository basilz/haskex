{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ch4.CsvChallenge where

import qualified Control.Foldl as F
import Control.Exception ( assert )
import Control.Monad ( MonadPlus(mzero) )
import Control.Comonad (Comonad (..))
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Csv
    ( (.!),
      decode,
      FromField(..),
      FromRecord(..),
      HasHeader(NoHeader) )
import Data.Text ( pack, strip, toLower, Text )
import Data.Time ( defaultTimeLocale, parseTimeM, Day )
import qualified Data.Vector as V
import GHC.Generics ( Generic )
import qualified Control.Comonad as F

data Gender = Male | Female deriving (Eq, Generic, Show)

data Person = Person {name :: !Text, gender :: !Gender, dob :: !Day}
  deriving (Generic, Show)

data Stats = Stats {femaleNum :: Int, maleNum :: Int, oldestPerson :: Maybe Text}
  deriving (Show)

data Stats' = Stats' {femaleNum' :: Int, maleNum' :: Int, oldestPerson' :: Maybe Person}
  deriving (Show)

instance Semigroup Stats' where
  Stats' f1 m1 (Just p1) <> Stats' f2 m2 (Just p2) = 
    Stats' (f1 + f2) (m1 + m2) (if dob p1 > dob p2 then Just p2 else Just p1) 
  Stats' _ _ Nothing <> s = s
  s <> Stats' _ _ Nothing = s

instance Monoid Stats' where
  mempty = Stats' 0 0 Nothing

mkStats' :: Person -> Stats'
mkStats' p@(Person _ Male _) = Stats' 0 1 (Just p)
mkStats' p@(Person _ Female _) = Stats' 1 0 (Just p)

instance Data.Csv.FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . B8.unpack

instance Data.Csv.FromField Gender where
  parseField s = case toLower (strip (pack (B8.unpack s))) of
    "male" -> pure Male
    "female" -> pure Female
    _ -> mzero

instance Data.Csv.FromRecord Person where
  parseRecord v = Person <$> v Data.Csv..! 0 <*> v Data.Csv..! 1 <*> v Data.Csv..! 2

readCsvFile :: FilePath -> IO (V.Vector Person)
readCsvFile fp = do
  csvData <- BL.readFile fp
  case Data.Csv.decode Data.Csv.NoHeader csvData of
    Left err -> error err
    Right pp -> return pp

countMales :: F.Fold Person Int
countMales = F.Fold step 0 id
  where 
    step n (Person _ Male _) = n + 1
    step n _ = n

countFemales :: F.Fold Person Int
countFemales = F.Fold step 0 id
  where 
    step n (Person _ Female _) = n + 1
    step n _ = n

oldest :: F.Fold Person (Maybe Text)
oldest = F.Fold step Nothing (name <$>)
  where
    step (Just p1) p2 = if dob p1 < dob p2 then Just p1 else Just p2
    step Nothing p2 = Just p2

stats :: F.Fold Person Stats
stats = Stats <$> countFemales <*> countMales <*> oldest

stats' :: V.Vector Person -> Stats'
stats' = V.foldMap mkStats'

main :: FilePath -> IO ()
main fp = do 
  v1 <- readCsvFile fp
  let suspend = F.fold (F.duplicate stats) v1
  v2 <- readCsvFile fp
  let final = F.fold suspend v2
  assert (oldestPerson final == oldestPerson (extract suspend)) $ print final
  -- print . F.fold (F.fold (F.duplicate stats) v1) $ v2
