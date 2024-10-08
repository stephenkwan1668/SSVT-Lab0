module Main where

import Test.QuickCheck
import Test.FitSpec

import Lab0


-- evenNumberList = [2,4,6,8]
-- evenNumber = 4
-- exampleString = "this is nice"

main :: IO ()
main = do 
  report defaultArgs { nMutants = 500 } properties



