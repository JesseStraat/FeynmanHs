import Data.List (splitAt)
import Ispermutation (isPermutation)
import Data.Hashable (Hashable, hashWithSalt)

data Object = Source String | Vertex String deriving (Show)
-- Syntax: Source {name} or Vertex {name}
{- Deprecated eq instance (in case it is needed in the future)
instance Eq Object where
    Source x == Source y = True
    Vertex x == Vertex y = x == y
    Source x == Vertex y = False
    x == y = False
-}
instance Hashable Object where
    hashWithSalt n (Source x) = hashWithSalt n ""
    hashWithSalt n (Vertex x) = hashWithSalt n ('v':x)



data Graph = Graph [(Object, Object)] deriving (Show)
instance Eq Graph where
    Graph xs == Graph ys = isPermutation xs ys

isInt :: (RealFrac a) => a  -> Bool
isInt x = x == fromInteger (round x)

graphGenerate :: [Object] -> Int -> [[(Object, Object)]]
-- [objects] -> # of edges -> [Feynman diagram]
graphGenerate [] 0 = [[]]
graphGenerate olist n
    | 2*n /= length olist = [[]]
    | otherwise           = [(olist !! 0, olist !! i) : list | i <- [1..(length olist - 1)], list <- (graphGenerate (pop (pop olist i) 0) (n-1))]
    where (x:xs) = olist

feynmanGenerate :: Int -> Int -> Int -> [Graph]
-- E -> V -> # of legs -> [Feynman diagram]
feynmanGenerate e v legs
    -- Complexity O(n!n^2) with n = length graphs
    | isInt p   = rmdups [Graph g | g <- graphs]
    | otherwise = []
    where elist = [Source (show x) | x <- [1..e]]
          vlist = [Vertex (show x) | x <- [1..v]]
          olist = elist ++ (replicateList vlist legs)
          p = (fromIntegral (e + legs*v))/2
          graphs = graphGenerate olist (round p)

replicateList :: [a] -> Int -> [a]
replicateList xs n = xs >>= replicate n

pop :: [a] -> Int -> [a]
pop [] _ = []
pop xs n
    | and[(n >= 0), (n < length xs)] = ys ++ zs
    | otherwise       = xs
    where (ys, z:zs) = splitAt n xs

rmdups :: (Eq a) => [a] -> [a]
-- Complexity O(n^2)
rmdups [] = []
rmdups (x:xs)
    | x `elem` xs = rmdups xs
    | otherwise   = x : rmdups xs

permutations :: [a] -> [[a]]
-- Complexity O(n!), deprecated
permutations [] = [[]]
permutations xs = [(xs !! i) : list | i <- [0..(length xs - 1)], list <- permutations (pop xs i)]