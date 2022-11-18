module Ispermutation
(
    isPermutation
) where

import qualified Data.HashMap.Strict as H (HashMap, empty, lookup, insert, delete, null)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)

histogram :: (Hashable a) => [a] -> H.HashMap a Int
histogram [] = H.empty
histogram (x:xs) = H.insert x (val + 1) hist
    where hist = histogram xs
          val = fromMaybe 0 (H.lookup x hist)

compareHistogram :: (Hashable a) => [a] -> H.HashMap a Int -> Bool
compareHistogram [] hist
    | H.null hist = True
    | otherwise   = False
compareHistogram (x:xs) hist
    | xfreq == 0 = False
    | otherwise  = compareHistogram xs newHist
    where xfreq = fromMaybe 0 (H.lookup x hist)
          newHist
            | xfreq == 1 = H.delete x hist
            | otherwise  = H.insert x (xfreq - 1) hist

isPermutation :: (Hashable a) => [a] -> [a] -> Bool
isPermutation xs ys = compareHistogram xs (histogram ys)