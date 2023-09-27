{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module FilrepApi(Config, Networks, config, network, getMiners) where

import Data.Text                (Text, intercalate, pack, unpack)
import Data.Proxy
import Data.Either              (fromRight)
import Network.HTTP.Client.TLS  (newTlsManager)
import Servant.API              ( (:>), Get, JSON, QueryParam )
import Servant.Client           (BaseUrl(..), ClientEnv, mkClientEnv,
                                 Scheme (Https), runClientM, client, HasClient (Client), ClientM, ClientError)

-- Local
import Filrep                   (Page, Miner, Pagination, DataBytes, miners,
                                 emptyPage, total, offset, limit, pagination)


getMiners :: Config -> IO [ Miner ]
getMiners config = do
  manager <- newTlsManager
  let clientEnv    = mkClientEnv manager $ baseUrl config
      minersClient = client minersApiProxy
  collectResults config minersClient clientEnv

collectResults :: Config -> Client ClientM MinersApi -> ClientEnv -> IO [Miner]
collectResults cfg client env = do
  let defaultLimit = pageSize cfg
  putStrLn $ "Default page size: " ++ show defaultLimit
  pages <- collectPages 0 defaultLimit
  return $ concatMap miners pages
    where

     -- TODO: add a Maybe 'total' argument to allow skipping errored pages?
     hasNext :: Page -> Bool
     hasNext page =
       let
         p = pagination page
       in
         total p > offset p + limit p

     collectPages :: Int -> Int -> IO [Page]
     collectPages offset lim = do
       putStrLn $ "Collecting page => offset: "
                    ++ show offset ++ " limit: " ++ show lim
       req <- runClientM (client (Just offset) (Just lim)) env
       case req of
         Left e -> do
           print e
           return []
         Right p ->
           if hasNext p then do
             let nextLimit = limit . pagination $ p
                 nextOffset = offset + nextLimit
             rest <- collectPages nextOffset nextLimit
             return $ p : rest
           else
             return [p]


type Error = String

type MinersApi =
  "v1" :> "miners" :> QueryParam "offset" Int :> QueryParam "limit" Int :> Get '[JSON] Page

minersApiProxy :: Proxy MinersApi
minersApiProxy = Proxy

-- TODO: Code Gen ADTs https://stackoverflow.com/a/44144476/2684881
config :: String -> Config
config "mainnet"  = Config (mkBaseUrl "api.filrep.io") 15 Mainnet
config "calibnet" = Config (mkBaseUrl "api.calibration.filrep.io") 30 Calibnet
config n          =
      error $ "fil-miners has no configuration for network: '" ++ n ++ "'"

mkBaseUrl :: String -> BaseUrl
mkBaseUrl h =
  BaseUrl { baseUrlScheme = Https
          , baseUrlHost = h
          , baseUrlPort = 443
          , baseUrlPath = "/api"
          }

mkDebugUrl :: String -> BaseUrl
mkDebugUrl _ =
  BaseUrl { baseUrlScheme = Https
          , baseUrlHost = "requestinspector.com"
          , baseUrlPort = 443
          , baseUrlPath = "/inspect/01h8gdanvcb4bpd8csv7w5thmm"
          }

data Config = Config
  { baseUrl :: BaseUrl
  , pageSize :: Int
  , network :: Networks
  } deriving (Show)

data Networks = Mainnet | Calibnet deriving (Show)
