module HW2.T2
  ( joinWith
  , splitOn
  ) where

import           Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List (intercalate)

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr add $ pure []
  where
    add c (x :| xs) | c == sep  = [] :| (x : xs)
                    | otherwise = (c : x) :| xs

joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ ([] :| []) = []
joinWith sep (x :| xs) = x ++ (sep : intercalate [sep] xs)
