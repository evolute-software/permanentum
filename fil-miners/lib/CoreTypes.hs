module CoreTypes
  ( Miner(..)
  , AttoFil, mkAttoFil
  , Bytes, mkBytes
  , UnixMs, mkUnixMs
  ) where

--
-- Canonical Miner Representation returned by the API and used by the DB

data Miner = Miner
  { filrep_id :: Integer
  , price :: Maybe AttoFil
  , region :: Maybe Region
  , iso2_code :: Maybe Iso2Code
  , rank :: Integer
  , status :: Bool
  , reachable :: Bool
  , free_space :: Maybe Bytes
  , minPieceSize :: Maybe Bytes
  , maxPieceSize :: Maybe Bytes
  , dealsTotal :: Integer -- Deals not important enough for smart construction
  , deals_pristine :: Integer
  , deals_slashed :: Integer
  , deals_terminated :: Integer
  , deals_fault_terminated :: Integer
  , created_at :: UnixMs
  , updated_at :: UnixMs
  } deriving (Show, Eq, Ord)

newtype AttoFil = AttoFil Integer deriving (Show, Eq, Ord)

mkAttoFil :: Integer -> AttoFil
mkAttoFil =
  constrainPositiveInt AttoFil

newtype Bytes = Bytes Integer deriving (Show, Eq, Ord)

mkBytes :: Integer -> Bytes
mkBytes =
  constrainPositiveInt Bytes

-- ISO2 country code based on the  ISO CC spec
newtype Iso2Code = Iso2Code String deriving (Show, Eq, Ord)

mkIso2Code :: String -> Iso2Code
mkIso2Code =
  constrainLength 2 2 Iso2Code

-- Timestamp resembling the microseconds passed since 1970-01-01 00:00:00+00
newtype UnixMs = UnixMs String deriving (Show, Eq, Ord)

mkUnixMs :: String -> UnixMs
mkUnixMs =
  constrainLength 2 2 UnixMs

constrainLength :: Int -> Int -> (String -> a) -> String -> a
constrainLength min max constructor s =
  if length s >= min && length s <= max then
    constructor s
  else
    error $
      "The passed string must be between " ++ show min ++ " and "
      ++ show max ++ " (both inclusive). Passed length: " ++ show (length s)


constrainPositiveInt :: (Integer -> a)-> Integer -> a
constrainPositiveInt constructor i =
  if i >= 0 then
    constructor i
  else
    error "No Negative integers allowed"

data Region = Asia
            | Europe
            | Africa
            | Oceania
            | NorthAmerica
            | CentralAmerica
            | SouthAmerica
            | Unknown
            deriving (Show, Eq, Ord)


