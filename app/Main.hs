{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (IOException, catch)
import qualified Web.Scotty as S
import System.Environment (lookupEnv)

import Web.Scotty hiding (catch)
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
              `catch` (\(_ :: IOException) -> return BL.empty)

  let wasteList =
        case decode NoHeader csvData of
          Left _ -> []
          Right rows -> V.toList (rows :: V.Vector Waste)

  putStrLn ("Records Loaded: " ++ show (length wasteList))

  -- ✅ FIXED PORT (Render compatible)
  portEnv <- lookupEnv "PORT"
  let port = maybe 10000 read portEnv

  putStrLn ("Starting server on port " ++ show port)

  S.scotty port $ do

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
            if null wasteList
              then 0
              else totalWaste / fromIntegral (length wasteList)
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