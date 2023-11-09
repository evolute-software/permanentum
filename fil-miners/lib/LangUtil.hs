module LangUtil
    ( maybeL
    , maybeR
    ) where

-- Convenience Stuff

maybeL :: Either a b -> Maybe a
maybeL (Left m) = Just m
maybeL (Right _) = Nothing

maybeR :: Either a b -> Maybe b
maybeR (Left _) = Nothing
maybeR (Right m) = Just m
