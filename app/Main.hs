module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  (filePath:_) <- getArgs
  healthXmlToCsv filePath
