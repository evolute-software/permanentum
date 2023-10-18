{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SpDb( connect
           , disconnect
           , runMigrations
           , persistMiners
           ) where

import           Control.Concurrent         (MVar, takeMVar)
import           Control.Monad              (void)
import qualified Database.PostgreSQL.Simple as P
import qualified Database.PostgreSQL.Simple.Migration as PSM
import           Data.FileEmbed             (embedDir, embedFile)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (pack)
import           Data.ByteString.UTF8       (toString)
import           System.FilePath            (takeFileName)
import           Text.Regex.TDFA            ((=~), Regex)
import           Data.String (fromString)

import qualified Filrep


type DbPassword = String
connect :: DbPassword  -> IO P.Connection
connect pw = do
  let ci = P.defaultConnectInfo { P.connectHost = "fil-sp-db"
                                , P.connectDatabase = "fil-sp-db"
                                , P.connectUser = "fil-sp-db"
                                , P.connectPassword = pw
                                }
  P.connect ci


disconnect :: P.Connection -> IO ()
disconnect conn = do
  putStrLn "Disconnecting fil-sp-db"
  P.close conn

hello :: P.Connection -> IO Int
hello conn = do
  [P.Only i] <- P.query_ conn "select 2 + 2"
  return i

migrationOptions :: PSM.MigrationOptions
migrationOptions =
  PSM.defaultOptions
    { PSM.optTransactionControl = PSM.TransactionPerRun
    , PSM.optVerbose = PSM.Verbose
    }

runMigrations :: P.Connection -> IO Bool
runMigrations conn = do
  tables <- P.query_ conn qShowTables :: IO [P.Only String]
  mapM_ (print . P.fromOnly) tables
  PSM.runMigrations conn migrationOptions migrations
  return True

migrations :: [ PSM.MigrationCommand ]
migrations =
  let
    toMigration :: (FilePath, ByteString) -> PSM.MigrationCommand
    toMigration (name, bs) = PSM.MigrationScript name bs
  in
    PSM.MigrationInitialization : map toMigration migrationFiles

-- TODO: add build deps for resources/db
migrationFiles :: [(FilePath, ByteString)]
migrationFiles = $(embedDir "resources/db/migrations")

persistMiners :: P.Connection -> MVar [Filrep.Miner] -> IO ()
persistMiners conn mv = do
  putStrLn "Waiting for miners to be fetched"
  miners <- takeMVar mv
  putStr $ "Got " ++  show (length miners) ++ " miners "
  (new, vanished) <- persistMiners' conn miners
  putStrLn $ "(" ++ show new ++ " new, " ++ show vanished ++ " vanished)"

persistMiners' :: P.Connection -> [Filrep.Miner] -> IO (Int, Int)
persistMiners' conn miners = do
  error "Implement this"


-- Here be Queries ------------------------------------------------------------

qPersistMiners :: P.Query
qPersistMiners = bsQuery $(embedFile "resources/db/queries/persist-miners.sql")

qShowTables :: P.Query
qShowTables = bsQuery $(embedFile "resources/db/queries/list-tables.sql")

bsQuery :: ByteString -> P.Query
bsQuery = fromString . toString
