module Main where

import System.IO
import ParsecExample(readEvalLoop)

main :: IO ()
main = do
  -- disable buffering to print the prompt immediately
  hSetBuffering stdout NoBuffering
  -- go into the read-eval-print loop
  readEvalLoop
