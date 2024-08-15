module Main where

import Lab0

main :: IO ()
main = do
    putStrLn "First 13 primes:"
    print $ take 13 primes

    putStrLn "Generating 5 random probabilities:"
    probsResult <- probs 5
    print probsResult

