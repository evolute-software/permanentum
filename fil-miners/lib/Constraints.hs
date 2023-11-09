{-# LANGUAGE NoImplicitPrelude #-}

module Constraints
  ( length
  , positive
  ) where

import qualified Prelude
import           Prelude hiding (length)


length :: Int -> Int -> (String -> a) -> String -> Either String a
length min max constructor s =
  let
    l = Prelude.length s
  in
  if l >= min && l <= max then
    Right $ constructor s
  else
    Left $
      "The passed string must be between " ++ show min ++ " and "
      ++ show max ++ " (both inclusive). Passed length: " ++ show l


positive :: (Integer -> a)-> Integer -> Either String a
positive constructor i =
  if i >= 0 then
    Right $ constructor i
  else
    Left "No Negative integers allowed"
