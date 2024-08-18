module Main where

import Lab0

evenNumberList = [2,4,6,8]

main :: IO ()
main = do

    -- Pre-defined functions 
    putStrLn "First 13 primes:"
    print $ take 13 primes

    putStrLn "Generating 5 random probabilities:"
    probsResult <- probs 5
    print probsResult

    -- 'all' function definition and usage -practice
    putStrLn "'all' function practice:"
    let areAllEven = all(\x -> rem x 2 == 0) evenNumberList
    print areAllEven

    -- takeWhile practice
    putStrLn "takeWhile practice:"
    let lessThanSixList = takeWhile (\ x -> x < 6) evenNumberList
    print lessThanSixList

