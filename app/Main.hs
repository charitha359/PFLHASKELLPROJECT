{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Aeson (ToJSON)
import GHC.Generics
import Data.List
import Data.Function (on)
import Data.Ord (Down(..))

-------------------------------------------------
-- DATA STRUCTURE
-------------------------------------------------

data Waste = Waste
  { country :: String
  , year :: Int
  , waste :: Double
  } deriving (Show, Generic)

instance ToJSON Waste

-------------------------------------------------
-- CSV PARSER (NO HEADER)
-------------------------------------------------

instance FromRecord Waste where
  parseRecord v
    | V.length v == 3 =
        Waste <$> v .! 0
              <*> v .! 1
              <*> v .! 2
    | otherwise = fail "Invalid CSV row"

-------------------------------------------------
-- MAIN
-------------------------------------------------

main :: IO ()
main = do

  putStrLn "Loading Waste Dataset..."

  csvData <- BL.readFile "data/waste.csv"

  case decode NoHeader csvData of
    Left err -> print err

    Right rows -> do

      let wasteList = V.toList (rows :: V.Vector Waste)

      putStrLn ("Records Loaded: " ++ show (length wasteList))
      putStrLn "Server running at http://localhost:3000"

      scotty 3000 $ do

-------------------------------------------------
-- ROOT
-------------------------------------------------

        get "/" $
          text "Global Waste Analytics Backend Running"

-------------------------------------------------
-- ALL DATA
-------------------------------------------------

        get "/data" $
          json wasteList

-------------------------------------------------
-- TOTAL RECORD COUNT
-------------------------------------------------

        get "/count" $
          json (length wasteList)

-------------------------------------------------
-- AVAILABLE YEARS (FOR SLIDER)
-------------------------------------------------

        get "/years" $ do
          let yearsList = nub $ map year wasteList
          json (sort yearsList)

-------------------------------------------------
-- GLOBAL WASTE TREND (YEARLY TOTAL)
-------------------------------------------------

        get "/yearly-total" $ do

          let yearsList = nub $ map year wasteList

          let totals =
                map (\y ->
                    (y,
                     sum [waste w | w <- wasteList, year w == y])
                ) yearsList

          json (sortOn fst totals)

-------------------------------------------------
-- TOP WASTE COUNTRIES
-------------------------------------------------

        get "/top-countries" $ do

          let countries = nub $ map country wasteList

          let totals =
                map (\c ->
                    (c,
                     sum [waste w | w <- wasteList, country w == c])
                ) countries

          json (take 10 $ sortOn (Down . snd) totals)

-------------------------------------------------
-- LEAST WASTE COUNTRIES
-------------------------------------------------

        get "/least-countries" $ do

          let countries = nub $ map country wasteList

          let totals =
                map (\c ->
                    (c,
                     sum [waste w | w <- wasteList, country w == c])
                ) countries

          json (take 10 $ sortOn snd totals)

-------------------------------------------------
-- COUNTRY TREND (MULTI YEAR)
-------------------------------------------------

        get "/country-trend" $ do

          let countries = nub $ map country wasteList

          let trends =
                map (\c ->
                    (c,
                     sortOn fst
                       [ (year w, waste w)
                       | w <- wasteList, country w == c ])
                ) countries

          json trends

-------------------------------------------------
-- GLOBAL AVERAGE WASTE
-------------------------------------------------

        get "/global-average" $ do

          let totalWaste = sum (map waste wasteList)

          let avgWaste =
                totalWaste / fromIntegral (length wasteList)

          json avgWaste

-------------------------------------------------
-- WORLD MAP DATA
-------------------------------------------------

        get "/map-data" $ do

          let countries = nub $ map country wasteList

          let totals =
                map (\c ->
                    (c,
                     sum [waste w | w <- wasteList, country w == c])
                ) countries

          json totals