data Object = Source String | Vertex String deriving (Show)
-- Syntax: Source {name} or Vertex {name}

data Propagator = Propagator (Object, Object)

data Graph = Graph [(Object, Object)]

isInt :: (RealFrac a) => a  -> Bool
isInt x = x == fromInteger (round x)

graphGenerate :: (Integral a) => [Object] -> a -> [Object]
graphGenerate olist _ = olist

feynmanGenerate :: Int -> Int -> Int -> [Object]
-- E -> V -> # of legs -> [Feynman diagram]
feynmanGenerate e v legs
    | isInt p   = graphGenerate olist (round p)
    | otherwise = []
    where elist = [Source (show x) | x <- [1..e]]
          vlist = [Vertex (show x) | x <- [1..v]]
          olist = elist ++ (replicateList vlist legs)
          p = (fromIntegral (2*e + legs*v))/2

replicateList :: [a] -> Int -> [a]
replicateList xs n = xs >>= replicate n