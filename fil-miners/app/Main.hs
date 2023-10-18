{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Main(main) where

import           Data.Text                  (Text, intercalate, pack, unpack)
import           System.Environment         (getEnv)
import           Control.Monad              (forever, unless, void)
import           Control.Concurrent         (forkIO, newMVar, newEmptyMVar
                                            , takeMVar , putMVar, isEmptyMVar
                                            , threadDelay, MVar, ThreadId, readMVar)
import           Control.Concurrent.Timer   (repeatedTimer, repeatedRestart, repeatedStart)
import           Control.Concurrent.Suspend (sDelay, mDelay)
import qualified System.Posix.Signals       as Signals
import qualified System.IO                  as SysIO
import           System.Exit                (die)

import qualified Filrep
import FilrepApi (Config (network), config, getMiners)
import qualified SpDb

spDbSecretPath :: FilePath
spDbSecretPath = "/run/secrets/fil-sp-db"

exit :: MVar () -> IO ()
exit exitMVar = do
  putStrLn "Exiting"
  takeMVar exitMVar

main :: IO ()
main = do

  -- Enforce LineBuffering even in daemon mode
  SysIO.hSetBuffering SysIO.stdout SysIO.LineBuffering

  (cfg, dbPw) <- getRuntime
  putStrLn $ makeStartupMsg dbPw cfg

  putStr "Trying to set up DB... "
  conn <- SpDb.connect dbPw
  putStrLn "Success!"

  putStrLn "Running migrations... "
  migrations <- SpDb.runMigrations conn
  if migrations then do
    putStrLn "Success"
  else do
    die "Fail"

  putStr "Trying to start fetch timer in a separate thread... "
  minersFetched <- newEmptyMVar :: IO (MVar [Filrep.Miner])
  timer <- repeatedTimer (fetchMiners cfg minersFetched) $ sDelay 30
  (status, thread) <- forkTimer $ repeatedRestart timer
  if status then do
    putStrLn "Success"
  else do
    die "Fail"
  putStrLn "Initialized. Entering Wait loop"

  putStr "Trying to start persistence... "
  timer2 <- repeatedTimer (SpDb.persistMiners conn minersFetched) $ sDelay 1
  void $ repeatedRestart timer2
  putStrLn "Initialized. Entering Wait loop"

  -- Exit on interupts
  exitMVar <-  newMVar ()
  Signals.installHandler Signals.sigINT (Signals.Catch (exit exitMVar))  Nothing
  let loop = do threadDelay 1_000_000
                exit <- isEmptyMVar exitMVar
                unless exit loop

  --getMiners cfg >>= print
  loop
  SpDb.disconnect conn

fetchMiners :: Config -> MVar [Filrep.Miner] -> IO ()
fetchMiners cfg mv= do
  putStrLn "Fetching miners"
  getMiners cfg >>= putMVar mv


forkTimer :: IO Bool -> IO (Bool, ThreadId)
forkTimer action = do
  statusMVar <- newEmptyMVar
  thread <- forkIO $ void $ do
                            started <- action
                            putMVar statusMVar started
  status <- takeMVar statusMVar
  pure (status, thread)

makeStartupMsg :: String -> Config -> String
makeStartupMsg dbPw cfg =
  let
    msgs = [ "'fil-miners' starting with: "
           , pack dbPw
           , pack $ show $ network cfg
           , pack $ show cfg
           ]

    msg = intercalate "\n" msgs
  in
    unpack msg

getRuntime :: IO (Config, DbPassword)
getRuntime = do
  putStrLn "Reading secrets"
  dbpw <- readFile spDbSecretPath

  putStrLn "Configuring from Environment"
  net <- getEnv "NET"
  let cfg = config net

  pure (cfg, dbpw)

-- ToDo: generic opaque types?
type DbPassword = String
