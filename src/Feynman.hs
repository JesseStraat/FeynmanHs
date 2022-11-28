module Feynman(
    feynmanGenerate
) where

import Ispermutation (isPermutation)
import Data.Hashable (Hashable, hashWithSalt)


-- Data types

data Object = Source | Vertex String deriving (Show)
-- Syntax: Source or Vertex {name}
instance Eq Object where
    Source == Source = True
    Vertex x == Vertex y = x == y
    Source == Vertex _ = False
    Vertex _ == Source = False
instance Hashable Object where
    hashWithSalt n (Source) = hashWithSalt n ""           -- All sources are the same, and should hash to the same value
    hashWithSalt n (Vertex x) = hashWithSalt n ('v':x)      -- The 'v' is added to ensure that the vertices can never have the same hash as the sources

data Graph = Graph [(Object, Object)] deriving (Show)       -- A Feynman diagram
instance Eq Graph where
    Graph xs == Graph ys = isPermutation xs ys              -- Permutations of Feynman diagrams are equivalent


-- The primary function of this script:

feynmanGenerate :: Int -> Int -> Int -> [Graph]
-- E -> V -> # of legs -> [Feynman diagram]
feynmanGenerate e v legs
    | isInt p   = rmdups [Graph g | g <- graphs]
    | otherwise = []
    where elist = [Source | _ <- [1..e]]
          vlist = [Vertex (show x) | x <- [1..v]]
          olist = elist ++ (replicateList vlist legs)
          p :: Double
          p = (fromIntegral (e + legs*v))/2
          graphs = graphGenerate olist (round p)


-- Some background functions

graphGenerate :: [Object] -> Int -> [[(Object, Object)]]    -- Generates Feynman diagrams without considering symmetries
-- [objects] -> # of edges -> [Feynman diagram]
graphGenerate [] 0 = [[]]
graphGenerate olist n
    | 2*n /= length olist = [[]]
    | otherwise           = [(olist !! 0, olist !! i) : list | i <- [1..(length olist - 1)], list <- (graphGenerate (pop (pop olist i) 0) (n-1))]

rmdups :: (Eq a) => [a] -> [a]
-- Complexity O(n^2)
rmdups [] = []
rmdups (x:xs)
    | x `elem` xs = rmdups xs
    | otherwise   = x : rmdups xs


-- Auxiliary functions

isInt :: (RealFrac a) => a  -> Bool
isInt x = x == fromInteger (round x)

replicateList :: [a] -> Int -> [a]
replicateList xs n = xs >>= replicate n

pop :: [a] -> Int -> [a]
pop [] _ = []
pop xs n
    | and[(n >= 0), (n < length xs)] = ys ++ zs
    | otherwise                     = xs
    where (ys, _:zs) = splitAt n xs