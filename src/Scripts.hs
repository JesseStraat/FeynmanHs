module Scripts(
    withEcho
) where

import System.IO (hGetEcho, hSetEcho, stdin)
import Control.Exception (bracket_)

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action