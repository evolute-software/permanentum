{-# LANGUAGE DataKinds #-}

module Main(main) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy as B
import System.Exit (exitFailure)

import Filrep (Page)

jsonFile :: FilePath
jsonFile = "miners-50.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

main :: IO ()
main = do
    putStrLn "Initializing Tests..."
    putStrLn "Reading data..."

    fixture <- getJSON
    putStrLn $ "Total Fixture size: " ++ show (B.length fixture)
    d <- (eitherDecode <$> getJSON) :: IO (Either String Page)
    case d of
        Left err -> do
            putStrLn err
            exitFailure
        Right _ -> do
            putStrLn "values Read"
