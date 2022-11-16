
data Object = Source String | Vertex String Int deriving (Show)
-- Syntax: Source {name} or Vertex {name} {# of legs}

data Propagator = Propagator String String deriving (Show)
-- Syntax: Propagator {name 1} {name 2}

data Graph = Graph [Object] [Propagator]

feynmanGenerate :: Int -> Int -> Int -> Graph
-- E -> V -> # of legs -> Feynman diagram