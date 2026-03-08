# Global Waste Analytics Framework

This project implements a waste data analytics backend using Haskell.

## Technologies
- Haskell
- Scotty Web Framework
- Cassava CSV Parser
- Aeson JSON Library

## Dataset
World Bank Global Waste Dataset

## API Endpoints

/                 -> Backend status  
/data             -> All dataset records  
/count            -> Total number of records  
/top-waste        -> Country with highest waste  
/average-waste    -> Average waste generation  
/eas-waste        -> East Asia data  
/europe-waste     -> Europe data  
/country?name=X   -> Search country waste data  
/region-summary   -> Region-wise waste totals