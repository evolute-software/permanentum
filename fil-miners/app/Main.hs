{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Data.Text                  (Text, intercalate, pack, unpack)
import           System.Environment         (getEnv)
import           Control.Monad              (forever, unless, void)
import           Control.Concurrent         (forkIO, newMVar, newEmptyMVar
                                            , takeMVar , putMVar, isEmptyMVar
                                            , threadDelay, MVar, ThreadId)
import           Control.Concurrent.Timer   (repeatedTimer, repeatedRestart)
import           Control.Concurrent.Suspend (sDelay, mDelay)
import qualified System.Posix.Signals       as Signals
import qualified System.IO                  as SysIO


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

  -- Exit on interupts
  exitMVar <-  newMVar ()
  Signals.installHandler Signals.sigINT (Signals.Catch (exit exitMVar))  Nothing

  putStrLn "Reading secrets"
  dbPw <- readFile spDbSecretPath

  putStrLn "Configuring from Environment"
  net <- getEnv "NET"

  let cfg = config net
  putStrLn $ makeGreeting dbPw net cfg

  putStr "Trying to start fetch timer... "
  timer <- repeatedTimer (putStrLn "Tick!") $ sDelay 1
  (status, thread) <- forkTimer $ repeatedRestart timer

  --statusMVar <- newEmptyMVar
  --thread <- forkIO $ void $ do
  --                          started <- repeatedRestart timer
  --                          putMVar statusMVar started

  --status <- takeMVar statusMVar
  putStrLn $ if status then "Success" else "Fail"

  timer2 <- repeatedTimer (putStrLn "Tick2!") $ sDelay 1
  void $ repeatedRestart timer2

  putStrLn "Initialized. Entering Wait loop"
  let loop = do threadDelay 1_000_000
                exit <- isEmptyMVar exitMVar
                unless exit loop
  loop

forkTimer :: IO Bool -> IO (Bool, ThreadId)
forkTimer action = do
  statusMVar <- newEmptyMVar
  thread <- forkIO $ void $ do
                            started <- action
                            putMVar statusMVar started
  status <- takeMVar statusMVar
  pure (status, thread)

makeGreeting :: String -> String -> Config -> String
makeGreeting dbPw net cfg =
  let
    msgs = [ "'fil-miners' starting with: "
           , pack dbPw
           , pack net
           , pack $ show cfg
           ]

    msg = intercalate "\n" msgs
  in
    unpack msg


-- TODO: Code Gen ADTs https://stackoverflow.com/a/44144476/2684881
config :: String -> Config
config "mainnet"  = Config "https://api.filrep.io/api" 15
config "calibnet" = Config "https://api.calibration.filrep.io/api" 30
config n          =
  error $ "fil-miners has no configuration for network: '" ++ n ++ "'"

data Config = Config
  { baseUrl :: Text
  , pageSize :: Int
  } deriving (Show)

