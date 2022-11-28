module Main (main) where

import Feynman (feynmanGenerate)
import System.IO (stdout, BufferMode(NoBuffering), hSetBuffering)
import Scripts (withEcho)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "(c) 2022 Jesse Straat"
    putStrLn "Welcome to the FeynmanHs test suite."
    putStrLn "To start, we would like to generate all Feynman diagrams."
    putStrLn "Please provide us with the necessary parameters."
    putStr "E = "
    estring <- getLine
    let e = (read estring :: Int)
    putStr "V = "
    vstring <- getLine
    let v = (read vstring :: Int)
    putStr "#legs = "
    legsstring <- getLine
    let legs = (read legsstring :: Int)
    putStrLn "We will now output all Feynman diagrams."
    let diagrams = feynmanGenerate e v legs
    printList diagrams
    putStrLn ""
    putStrLn "Finished!"
    putStr "Press 'enter' to close the program."
    foo <- withEcho False getLine
    return ()

printList :: (Show a) => [a] -> IO()
printList [] = return()
printList (x:xs) = do
    putStrLn (show x)
    printList xs