module Ispermutation
(
    isPermutation
) where

import qualified Data.HashTable.IO as H
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)

type HashTable k v = H.BasicHashTable k v

histogram :: (Hashable a) => [a] -> IO(HashTable a Int)
histogram [] = do
    ht <- H.new
    return ht
histogram (x:xs) = do
    ht <- histogram xs
    val <- H.lookup ht x
    H.insert ht x (fromMaybe 0 val + 1)
    return ht

compareHistogram :: (Hashable a) => [a] -> IO(HashTable a Int) -> IO(Bool)
compareHistogram [] hio = do
    ht <- hio
    hlist <- H.toList ht
    if hlist == []
        then return True
        else return False
compareHistogram (x:xs) hio = do
    ht <- hio
    xval <- H.lookup ht x
    let xfreq = fromMaybe 0 xval
    if xfreq == 0
        then return False
        else do
            if xfreq == 1
                then H.delete ht x
                else H.insert ht x (xfreq - 1)
            compareHistogram xs (return ht)

isPermutation :: (Hashable a) => [a] -> [a] -> IO(Bool)
isPermutation xs ys = compareHistogram xs (histogram ys)