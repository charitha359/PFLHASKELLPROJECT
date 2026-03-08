{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Aeson (ToJSON)
import GHC.Generics
import Data.List (maximumBy, sortOn)
import Data.Function (on)
import Data.Ord (Down(..))

-- Waste data structure
data Waste = Waste
  { country_code :: String
  , region :: String
  , country :: String
  , income_group :: String
  , gdp_per_capita :: Double
  , population :: Double
  , waste_tons :: Double
  } deriving (Show, Generic)

instance ToJSON Waste

-- CSV parser
instance FromRecord Waste where
  parseRecord v
    | V.length v == 7 =
        Waste <$> v .! 0
              <*> v .! 1
              <*> v .! 2
              <*> v .! 3
              <*> v .! 4
              <*> v .! 5
              <*> v .! 6
    | otherwise = fail "Invalid row"

main :: IO ()
main = do

  putStrLn "Loading CSV dataset..."

  csvData <- BL.readFile "data/globalwastedata.csv"

  case decode NoHeader csvData of
    Left err -> print err

    Right rows -> do

      let wasteList = V.toList (rows :: V.Vector Waste)

      putStrLn ("Records Loaded: " ++ show (length wasteList))
      putStrLn "Server running on http://localhost:3000"

      scotty 3000 $ do

        -- Root
        get "/" $
          text "Haskell Waste Analytics Backend Running"

        -- All data
        get "/data" $
          json wasteList

        -- Total records
        get "/count" $
          json (length wasteList)

        -- Top waste country
        get "/top-waste" $
          json (maximumBy (compare `on` waste_tons) wasteList)

        -- Top 5 countries
        get "/top5" $
          json (take 5 $ sortOn (Down . waste_tons) wasteList)

        -- Least 5 countries
        get "/least5" $
          json (take 5 $ sortOn waste_tons wasteList)

        -- Average waste
        get "/average-waste" $ do
          let totalWaste = sum (map waste_tons wasteList)
          let avgWaste = totalWaste / fromIntegral (length wasteList)
          json avgWaste

        -- Region summary
        get "/region-summary" $ do
          let regions = ["EAS","ECS","LCN","MEA","NAC","SAS","SSF"]
          let totals =
                map (\r ->
                    (r, sum [waste_tons w | w <- wasteList, region w == r])
                ) regions
          json totals

        -- Waste vs Population
        get "/waste-population" $ do
          let result =
                map (\w ->
                    (country w, population w, waste_tons w)
                ) wasteList
          json result

        -- Waste vs GDP
        get "/waste-gdp" $ do
          let result =
                map (\w ->
                    (country w, gdp_per_capita w, waste_tons w)
                ) wasteList
          json result

        -- Global distribution
        get "/global-distribution" $ do
          let totalWaste = sum (map waste_tons wasteList)
          let regions = ["EAS","ECS","LCN","MEA","NAC","SAS","SSF"]
          let distribution =
                map (\r ->
                    (r, sum [waste_tons w | w <- wasteList, region w == r])
                ) regions
          json (totalWaste, distribution)