module Lab0 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck




infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    putStrLn "p:"
    print p
    ps <- probs (n-1)
    putStrLn "ps:"
    print ps
    return (p:ps)

-- squaredList :: [Integer] -> [Integer]
-- squaredList list = map (\x -> x*x) list

sumUpList :: [Integer] -> Integer
sumUpList [] = 0
sumUpList (x:xs) = x + sumUpList xs


reversibleStream :: [Integer]
reversibleStream = undefined

reversal :: Integer -> Integer
reversal = read . reverse . show

rot13 :: [Char] -> [Char]
rot13 = undefined

consecutive101Prime :: Integer
consecutive101Prime = undefined

euler9 :: Integer
euler9 = undefined

euler10 :: Integer
euler10 = undefined

euler49 :: Integer
euler49 = undefined

-- "map" usage
powerOfThreeList :: [Integer] -> [Integer]
powerOfThreeList list = map (\x -> x * x * x) list

-- "filter" usage
filterOdds :: [Integer] -> [Integer]
filterOdds oddNumbers = filter (isOdd) oddNumbers

-- "any" usage
anyNegative :: [Integer] -> Bool
anyNegative numbersList = any (<0) numbersList

isOdd :: Integer -> Bool
isOdd x = mod x 2 /= 0 

isEven :: Integer -> Bool
isEven x = mod x 2 == 0

reverseString :: String -> String
reverseString elements = reverse elements

reverseNumList :: [Integer] -> [Integer]
reverseNumList list = reverse list

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

reverseList :: [a] -> [a]
reverseList [] = []  -- Base case: the reverse of an empty list is an empty list
reverseList (x:xs) = reverseList xs ++ [x]  -- Recursive case: reverse the tail and append the head at the end

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing            -- If the second number is 0, return Nothing (no result).
safeDivide x y = Just (x `div` y)   -- Otherwise, return the result wrapped in Just.



-- equal :: Int -> Double -> Bool
-- equal fun1 fun2 = mod fun1 fun2 == 0

-- -- sumedUpCubedList :: Integer -> [Integer] -> Integer
-- -- sumedUpCubedList = cubes . sumUpList

-- -- sumUpList :: [Integer] -> 

  -- Function to generate the list of cubes
squaredList :: [Int]
squaredList = map (\x -> x * x) [1..]

-- Function to sum up the first n cubes
squares :: Int -> Int
squares n = sum (take n squaredList)

-- Function to calculate the result of the equation
rightFirstEquation :: Int -> Double
rightFirstEquation n = fromIntegral (n * (n + 1) * (2 * n + 1)) / 6

-- Function to compare if cubes result and rightFirstEquation are equal
equal1 :: Int -> Bool
equal1 n = fromIntegral (squares n) == rightFirstEquation n

cubedList :: [Int] 
cubedList = map(\x -> x * x * x) [1..]

cubes :: Int -> Int
cubes n = sum (take n cubedList)

rightSecondEquation :: Int -> Double
rightSecondEquation n = fromIntegral (n * (n + 1) `div` 2) ^2

-- Function to compare if cubes result and rightFirstEquation are equal
equal2 :: Int -> Bool
equal2 n = fromIntegral (cubes n) == rightSecondEquation n

prop_squaresEqualToRightFirstEquation :: Int -> Bool
prop_squaresEqualToRightFirstEquation n = equal1 n

prop_squaresEqualToRightSecondEquation :: Int -> Bool
prop_squaresEqualToRightSecondEquation n = equal2 n

isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\x -> n `rem` x /= 0) [2..(floor . sqrt . fromIntegral $ n)]

primes :: [Integer]
primes = filter isPrime [2..]

productOfPrimes :: [Integer] -> Integer
productOfPrimes = foldr (*) 1

counterexamples :: [([Integer], Integer)]
counterexamples = findCounterexamples [] primes

findCounterexamples :: [Integer] -> [Integer] -> [([Integer], Integer)]
findCounterexamples currentPrimes (nextPrime:remainingPrimes) =
  let
    newPrimes = currentPrimes ++ [nextPrime]
    productPlusOne = productOfPrimes newPrimes + 1
  in
    if not (isPrime productPlusOne)
    then (newPrimes, productPlusOne) : findCounterexamples newPrimes remainingPrimes
    else findCounterexamples newPrimes remainingPrimes

main :: IO ()
main = do
  print $ take 10 counterexamples  