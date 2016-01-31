{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Internal as BSI
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map                 as M
import qualified Data.Vector              as V
-- cassava
import           Data.Csv
-- just a parser, using it for 'integer'
import           Text.Trifecta

-- This is more manual than I'd like as I'm
-- accustomed to wielding more kit, but hopefully
-- it gets the idea across.

-- ratio to seconds
type Hours = Integer
type Minutes = Integer
type Client = ByteString

minutes = 60
hours = minutes * 24

data ClockTime =
  ClockTime Hours Minutes
  deriving Eq

instance Show ClockTime where
  show (ClockTime h m) = show h ++ "h " ++ show m ++ "m"

-- Prelude> merge (10, 59) (10, 0)
-- (20,59)
-- Prelude> merge (10, 59) (10, 1)
-- (21,0)
merge :: ClockTime
      -> ClockTime
      -> ClockTime
merge (ClockTime h m) (ClockTime h' m') =
  let minutesTot = m + m'
      (leftovers, newMins) = quotRem minutesTot 60
  in ClockTime (h + h' + leftovers) newMins

instance Monoid ClockTime where
  mempty = ClockTime 0 0
  mappend = merge

-- This would be utterly unnecessary if I used `validation`
mergeErrors :: Result Hours
            -> Result Minutes
            -> Either [String] ClockTime
mergeErrors e e' =
  case (e, e') of
    (Failure d, Failure d') -> Left [show d, show d']
    (Success _, Failure d') -> Left [show d']
    (Failure d, Success _) -> Left [show d]
    -- glorified liftA2 (,) for `validation`
    (Success a, Success b) -> Right (ClockTime a b)

-- secondsToDiffTime
extractHoursMinutes :: M.Map ByteString ClockTime
                    -> V.Vector ByteString
                    -> Either [String] (M.Map ByteString ClockTime)
extractHoursMinutes m r =
  case ((V.!?) r 8, (V.!?) r 13) of
    (Just k, Just hourMinuteBs') ->
      let [recHours, recMinutes] = BL.split (BSI.c2w ':') hourMinuteBs'
          eHour = parseByteString integer mempty (BL.toStrict recHours)
          eMinute = parseByteString integer mempty (BL.toStrict recMinutes)
      in fmap (\v -> M.insert k v m) (mergeErrors eHour eMinute)
    (_, _) -> Left ["record didn't have at least 14 elements in: " ++ show r]

main :: IO ()
main = do
  csvData <- BL.readFile "data.csv"
  let v = decode NoHeader csvData :: Either String (V.Vector (V.Vector ByteString))
      m = M.empty
  case v of
    Left s -> print s
    Right vs ->
      case traverse (extractHoursMinutes m) vs of
        (Left errors) -> putStrLn ("Error(s) occurred: " ++ concat errors)
        (Right records) -> do
          print records
          print $ V.foldr (M.unionWith mappend) mempty records
