{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Filrep(Page(..), Miner(..), Pagination(..), Region, DataBytes, emptyPage) where

import qualified Data.Map.Strict as Map
import Data.Text (pack, unpack)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), (.=), object, (.:?), (.:!))
import Data.Aeson.Types (prependFailure, typeMismatch, Parser)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific, toBoundedInteger, scientific)

data Page = Page
  { miners :: [Miner]
  , pagination :: Pagination
  } deriving (Show, Eq, Ord)

emptyPage :: Page
emptyPage = Page [] (Pagination 0 0 0)

data Miner = Miner
  { id :: Integer
  , price :: Maybe AttoFilString
  , minPieceSize :: Maybe DataBytesStr
  , region :: Maybe Region
  , isoCode :: Maybe Iso2Code
  , scores :: Scores
  , rawPower :: DataBytesStr
  , goldenPath :: GoldenPath
  , rank :: String
  , status :: Bool
  , reachability :: String
  , energy :: Energy
  , tag :: Map.Map String (Maybe String)
  , creditScore :: Maybe Value
  , qualityAdjPower :: String
  , freeSpace :: Maybe DataBytesStr
  , score :: MaybeScalar
  , uptimeAverage :: Double
  , maxPieceSize :: Maybe String
  , verifiedPrice :: Maybe AttoFilString
  , regionRank :: String
  , storageDeals :: StorageDeals
  , address :: MinerAddress
  } deriving (Show, Eq, Ord)

data Pagination = Pagination
  { total :: Int
  , limit :: Int
  , offset :: Int
  } deriving (Show, Eq, Ord)

data Scores = Scores
  { committedSectorsProofs :: MaybeScalar
  , total :: MaybeScalar
  , uptime :: MaybeScalar
  , storageDeals :: MaybeScalar
  } deriving (Show, Eq, Ord)


data MaybeScalar = JustStr String | JustNum Scientific | Null
  deriving (Show, Eq, Ord)

instance ToJSON MaybeScalar where
  toJSON (JustStr u) = String (pack u)
  toJSON (JustNum n) = Number n
  toJSON Filrep.Null = Data.Aeson.Null

instance FromJSON MaybeScalar where
  parseJSON :: Value -> Parser MaybeScalar
  parseJSON (String s) =
        return $ JustStr $ unpack s
  parseJSON (Number n) =
        return $ JustNum n
  parseJSON Data.Aeson.Null =
        return Filrep.Null
  parseJSON invalid =
        prependFailure "parsing Score failed, "
            (typeMismatch "Object" invalid)

data StorageDeals = StorageDeals
  { slashed :: Maybe Integer
  , recent30days :: Maybe Integer
  , total :: Maybe Integer
  , terminated :: Maybe Integer
  , dataStored :: Maybe DataBytesStr
  , averagePrice :: Maybe AttoFilString
  , successRate :: Maybe String
  , noPenalties :: Maybe Integer
  , faultTerminated :: Maybe Integer
  } deriving (Show, Eq, Ord)

data Energy = Energy
  { recs :: Maybe Value
  , pageUrl :: Maybe Value
  } deriving (Show, Eq, Ord)

data GoldenPath = GoldenPath
  { storageDealSuccessRate :: Bool
  , faultsBelowThreshold :: Bool
  , retrievalDealSuccessRate :: Bool
  , fastRetrieval :: Maybe Value
  , verifiedDealNoPrice :: Bool
  } deriving (Show, Eq, Ord)

type MinerAddress = String
type AttoFilString = String
type AttoFil = Integer
type DataBytesStr = String
type DataBytes = Integer
type Iso2Code = String
type Region = String

instance ToJSON Energy where
  toJSON Energy{..} = object
    [ "recs" .= recs
    , "pageUrl" .= pageUrl
    ]

instance ToJSON GoldenPath where
  toJSON GoldenPath{..} = object
    [ "storageDealSuccessRate" .= storageDealSuccessRate
    , "faultsBelowThreshold" .= faultsBelowThreshold
    , "retrievalDealSuccessRate" .= retrievalDealSuccessRate
    , "fastRetrieval" .= fastRetrieval
    , "verifiedDealNoPrice" .= verifiedDealNoPrice
    ]

instance ToJSON Miner where
  toJSON Miner{..} = object
    [ "id" .= id
    , "price" .= price
    , "minPieceSize" .= minPieceSize
    , "region" .= region
    , "isoCode" .= isoCode
    , "scores" .= scores
    , "rawPower" .= rawPower
    , "goldenPath" .= goldenPath
    , "rank" .= rank
    , "status" .= status
    , "reachability" .= reachability
    , "energy" .= energy
    , "tag" .= tag
    , "creditScore" .= creditScore
    , "qualityAdjPower" .= qualityAdjPower
    , "freeSpace" .= freeSpace
    , "score" .= score
    , "uptimeAverage" .= uptimeAverage
    , "maxPieceSize" .= maxPieceSize
    , "verifiedPrice" .= verifiedPrice
    , "regionRank" .= regionRank
    , "storageDeals" .= storageDeals
    , "address" .= address
    ]

instance ToJSON Page where
  toJSON Page{..} = object
    [ "miners" .= miners
    , "pagination" .= pagination
    ]

instance ToJSON Pagination where
  toJSON Pagination{..} = object
    [ "total" .= total
    , "limit" .= limit
    , "offset" .= offset
    ]

instance ToJSON Scores where
  toJSON Scores{..} = object
    [ "committedSectorsProofs" .= committedSectorsProofs
    , "total" .= total
    , "uptime" .= uptime
    , "storageDeals" .= storageDeals
    ]

instance ToJSON StorageDeals where
  toJSON StorageDeals{..} = object
    [ "slashed" .= slashed
    , "recent30days" .= recent30days
    , "total" .= total
    , "terminated" .= terminated
    , "dataStored" .= dataStored
    , "averagePrice" .= averagePrice
    , "successRate" .= successRate
    , "noPenalties" .= noPenalties
    , "faultTerminated" .= faultTerminated
    ]

instance FromJSON Energy where
  parseJSON (Object v) = do
    recs <- v .: "recs"
    pageUrl <- v .: "pageUrl"
    pure $ Energy{..}
  parseJSON invalid = do
    prependFailure "parsing Energy failed, "
      (typeMismatch "Object" invalid)

instance FromJSON GoldenPath where
  parseJSON (Object v) = do
    storageDealSuccessRate <- v .: "storageDealSuccessRate"
    faultsBelowThreshold <- v .: "faultsBelowThreshold"
    retrievalDealSuccessRate <- v .: "retrievalDealSuccessRate"
    fastRetrieval <- v .: "fastRetrieval"
    verifiedDealNoPrice <- v .: "verifiedDealNoPrice"
    pure $ GoldenPath{..}
  parseJSON invalid = do
    prependFailure "parsing GoldenPath failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Miner where
  parseJSON (Object v) = do
    id <- v .: "id"
    price <- v .: "price"
    minPieceSize <- v .: "minPieceSize"
    region <- v .: "region"
    isoCode <- v .: "isoCode"
    scores <- v .: "scores"
    rawPower <- v .: "rawPower"
    goldenPath <- v .: "goldenPath"
    rank <- v .: "rank"
    status <- v .: "status"
    reachability <- v .: "reachability"
    energy <- v .: "energy"
    tag <- v .: "tag"
    creditScore <- v .: "creditScore"
    qualityAdjPower <- v .: "qualityAdjPower"
    freeSpace <- v .: "freeSpace"
    score <- v .: "score"
    uptimeAverage <- v .: "uptimeAverage"
    maxPieceSize <- v .: "maxPieceSize"
    verifiedPrice <- v .: "verifiedPrice"
    regionRank <- v .: "regionRank"
    storageDeals <- v .: "storageDeals"
    address <- v .: "address"
    pure $ Miner{..}
  parseJSON invalid = do
    prependFailure "parsing Miner failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Page where
  parseJSON (Object v) = do
    miners <- v .: "miners"
    pagination <- v .: "pagination"
    pure $ Page{..}
  parseJSON invalid = do
    prependFailure "parsing Page failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Pagination where
  parseJSON (Object v) = do
    total <- v .: "total"
    limit <- v .: "limit"
    oField <- v .:! "offset"
    let offset =
          fromMaybe 0 oField
    pure $ Pagination{..}
  parseJSON invalid = do
    prependFailure "parsing Pagination failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Scores where
  parseJSON (Object v) = do
    committedSectorsProofs <- v .: "committedSectorsProofs"
    total <- v .: "total"
    uptime <- v .: "uptime"
    storageDeals <- v .: "storageDeals"
    pure $ Scores{..}
  parseJSON invalid = do
    prependFailure "parsing Scores failed, "
      (typeMismatch "Object" invalid)

instance FromJSON StorageDeals where
  parseJSON (Object v) = do
    slashed <- v .: "slashed"
    recent30days <- v .: "recent30days"
    total <- v .: "total"
    terminated <- v .: "terminated"
    dataStored <- v .: "dataStored"
    averagePrice <- v .: "averagePrice"
    successRate <- v .: "successRate"
    noPenalties <- v .: "noPenalties"
    faultTerminated <- v .: "faultTerminated"
    pure $ StorageDeals{..}
  parseJSON invalid = do
    prependFailure "parsing StorageDeals failed, "
      (typeMismatch "Object" invalid)

