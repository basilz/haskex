{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ch4.QuotesStats where

import qualified Control.Scanl as S
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy as BL (readFile)
import Data.Csv (FromField (..), FromNamedRecord, decodeByName)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import GHC.Generics (Generic)
import Protolude (state)

data QuoteData = QuoteData
  { day :: Day,
    volume :: Int,
    open :: Double,
    close :: Double,
    high :: Double,
    low :: Double
  }
  deriving (Generic, FromNamedRecord)

instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

data QField = Open | Close | High | Low | Volume
  deriving (Eq, Ord, Show, Enum, Bounded)

data Stats = Stats
  { avg :: Double,
    min :: Double,
    max :: Double,
    minChanged :: Day,
    maxChanged :: Day
  }
  deriving (Show)

field2fun :: QField -> QuoteData -> Double
field2fun Open = open
field2fun Close = close
field2fun High = high
field2fun Low = low
field2fun Volume = fromIntegral . volume

-- work :: FilePath -> IO ()
-- work fp = do
--   csvData <- BL.readFile fp
--   case decodeByName csvData of
--     Left err -> putStrLn err
--     Right (_, quotes) -> print . stats $ quotes

makeScan :: s -> (a -> s -> (b, s)) -> S.Scan a b
makeScan s0 f = S.Scan (state . f) s0

slidingWindow :: Int -> S.Scan a (Maybe [a])
slidingWindow l = makeScan (1, []) $ \x (n, xs) ->
  let xs' = take l $ x : xs
      r = if n >= l then Just xs' else Nothing
   in (r, (n + 1, xs'))

slidingWindow' :: Int -> S.Scan a (Maybe [a])
slidingWindow' l = S.Scan (state . f) (1, [])
  where
    f x (n, xs) =
      let xs' = take l $ x : xs
          r = if n >= l then Just xs' else Nothing
       in (r, (n + 1, xs'))
