module Main where

import Prelude hiding (putStrLn, getContents, readFile)
import Lib (run)
import Data.Text.Lazy.IO (putStrLn, getContents)

main :: IO ()
main = getContents >>= putStrLn . run
