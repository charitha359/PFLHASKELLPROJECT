{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Aeson (ToJSON)
import GHC.Generics
import Data.List
import Data.Ord (Down(..))

import Network.Wai.Middleware.Static

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
-- CSV PARSER
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
-- STATIC FILE SERVER
-------------------------------------------------

        middleware $ staticPolicy (noDots >-> addBase ".")

-------------------------------------------------
-- ROOT DASHBOARD
-------------------------------------------------

        get "/" $ do
          setHeader "Content-Type" "text/html"
          file "index.html"

-------------------------------------------------
-- ALL DATA
-------------------------------------------------

        get "/data" $
          json wasteList

-------------------------------------------------
-- RECORD COUNT
-------------------------------------------------

        get "/count" $
          json (length wasteList)

-------------------------------------------------
-- AVAILABLE YEARS
-------------------------------------------------

        get "/years" $ do
          let yearsList = sort . nub $ map year wasteList
          json yearsList

-------------------------------------------------
-- GLOBAL WASTE TREND
-------------------------------------------------

        get "/yearly-total" $ do

          let yearsList = sort . nub $ map year wasteList

          let totals =
                map (\y ->
                    (y,
                     sum [waste w | w <- wasteList, year w == y])
                ) yearsList

          json totals

-------------------------------------------------
-- TOP COUNTRIES
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
-- LEAST COUNTRIES
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
-- COUNTRY TREND
-------------------------------------------------

        get "/country-trend" $ do

          let countries = nub $ map country wasteList

          let trends =
                map (\c ->
                    (c,
                     sortOn fst
                       [ (year w, waste w)
                       | w <- wasteList
                       , country w == c ])
                ) countries

          json trends

-------------------------------------------------
-- GLOBAL AVERAGE
-------------------------------------------------

        get "/global-average" $ do

          let totalWaste = sum (map waste wasteList)

          let avgWaste =
                totalWaste / fromIntegral (length wasteList)

          json avgWaste

-------------------------------------------------
-- MAP DATA
-------------------------------------------------

        get "/map-data" $ do

          let countries = nub $ map country wasteList

          let totals =
                map (\c ->
                    (c,
                     sum [waste w | w <- wasteList, country w == c])
                ) countries

          json totals