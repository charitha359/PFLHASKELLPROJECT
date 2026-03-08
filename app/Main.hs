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
import Data.Char (toLower)

-- Data structure
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

-- CSV parser (NoHeader)
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
    Left err -> do
      putStrLn "CSV Parse Error:"
      print err

    Right rows -> do
      let wasteList = V.toList (rows :: V.Vector Waste)

      putStrLn ("Total records loaded: " ++ show (length wasteList))

      scotty 3000 $ do

        -- Root route
        get "/" $
          text "PFL Haskell Waste Analytics Backend Running"

        -- All data
        get "/data" $
          json wasteList

        -- Count
        get "/count" $
          json (length wasteList)

        -- Top waste country
        get "/top-waste" $ do
          let top = maximumBy (compare `on` waste_tons) wasteList
          json top

        -- Top N waste countries (WITHOUT param)
        get "/top" $ do
          n <- queryParam "n" :: ActionM Int
          let sorted = take n $ reverse $ sortOn waste_tons wasteList
          json sorted

        -- Average waste
        get "/average-waste" $ do
          let total = sum (map waste_tons wasteList)
          let avg = if null wasteList then 0 else total / fromIntegral (length wasteList)
          json avg

        -- East Asia
        get "/eas-waste" $ do
          let eas = filter (\w -> region w == "EAS") wasteList
          json eas

        -- Europe
        get "/europe-waste" $ do
          let ecs = filter (\w -> region w == "ECS") wasteList
          json ecs

        -- Country search
        get "/country" $ do
          cname <- queryParam "name"
          let result =
                filter (\w -> map toLower (country w) == map toLower cname) wasteList
          json result

        -- Region summary (for graphs)
        get "/region-summary" $ do
          let regions = ["EAS","ECS","LCN","MEA","NAC","SAS","SSF"]

          let totals =
                map (\r ->
                  (r, sum [waste_tons w | w <- wasteList, region w == r])
                ) regions

          json totals