import Data.List (splitAt)

data Object = Source String | Vertex String deriving (Show)
-- Syntax: Source {name} or Vertex {name}
instance Eq Object where
    Source x == Source y = x == y
    Vertex x == Vertex y = x == y
    Source x == Vertex y = False

data Graph = Graph [(Object, Object)] deriving (Show)

isInt :: (RealFrac a) => a  -> Bool
isInt x = x == fromInteger (round x)

graphGenerate :: [Object] -> Int -> [[(Object, Object)]]
-- [objects] -> # of edges -> [Feynman diagram]
graphGenerate [] 0 = [[]]
graphGenerate olist n
    | 2*n /= length olist = [[]]
    | otherwise           = [(olist !! 0, olist !! i) : list | i <- [1..(length olist - 1)], list <- (graphGenerate (pop (pop olist i) 0) (n-1))]
    where (x:xs) = olist

feynmanGenerate :: Int -> Int -> Int -> [[(Object, Object)]]
-- E -> V -> # of legs -> [Feynman diagram]
feynmanGenerate e v legs
    | isInt p   = rmdups(graphGenerate olist (round p))
    | otherwise = []
    where elist = [Source (show x) | x <- [1..e]]
          vlist = [Vertex (show x) | x <- [1..v]]
          olist = elist ++ (replicateList vlist legs)
          p = (fromIntegral (e + legs*v))/2

replicateList :: [a] -> Int -> [a]
replicateList xs n = xs >>= replicate n

pop :: [a] -> Int -> [a]
pop [] _ = []
pop xs n
    | and[(n >= 0), (n < length xs)] = ys ++ zs
    | otherwise       = xs
    where (ys, z:zs) = splitAt n xs

rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x:xs)
    | x `elem` xs = rmdups xs
    | otherwise   = x : rmdups xs