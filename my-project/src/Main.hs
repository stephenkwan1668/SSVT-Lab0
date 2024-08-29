module Main where

import Lab0
    ( primes,
      (-->),
      probs,
      powerOfThreeList,
      filterOdds,
      anyNegative,
      isOdd,
      isEven,
      reverseString,
      reverseNumList,
      factorial,
      reverseList,
      forall, safeDivide )

evenNumberList = [2,4,6,8]
evenNumber = 4
exampleString = "this is nice"

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

    print $ take 3 (powerOfThreeList[1,2,3])
    
    print $ isOdd evenNumber
    print $ isEven evenNumber

    print $ take 5 (filterOdds[1,2,3,4,5,6])

    print $ anyNegative evenNumberList

    print $ reverseString exampleString

    print $ reverseNumList evenNumberList

    print $ factorial 4

    print $ reverseList evenNumberList

    print (True --> False)  -- Should print False
    -- because not(true) || false is false
    
    print (False --> True)  -- Should print True
    print (True --> True)   -- Should print True
    print (False --> False) -- Should print True

    print (True && True)

    -- print $ forall evenNumberList isOdd
    let result = forall [2, 4, 6, 8] odd
    print result

    -- This could be used as such if it is only "forall = all"
    -- let result = forall even [2, 4, 6, 8]
    -- print result

    putStrLn "from main, random numbers"
    randomNumbers <- probs 5
    print randomNumbers

    putStrLn "Enter your name:"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")

    let divideResult = safeDivide 5 0
    case divideResult of
      Nothing -> putStrLn("Can not divide by 0")
      Just value -> putStrLn ("safeDivide result: " ++ show value)
    

    let divideResult2 = safeDivide 5 1
    putStrLn $ "safeDivide result: " ++ maybe "Division by zero" show divideResult2

    


