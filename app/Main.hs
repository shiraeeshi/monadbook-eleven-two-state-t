module Main where

import Lib

main :: IO ()
main = do
  let
    state = AppState 100 2 3
    --state = AppState 100 0 3
    calculation = someFunction
    result = runStateT calculation state
  putStrLn $ "result: " ++ (show result)
