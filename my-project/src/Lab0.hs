module Lab0 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 :filter prime [3..] 

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

squaredList :: [Integer] -> [Integer]
squaredList list = map (\x -> x*x) list

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

-- Define a list with map, assign, and filtering
funList :: [Integer]
funList = [x * 2 | x <- [10..13], x /= 11, x /= 12]

-- Practice of function application, instead of having to use (), $ can be used
addTwoNumbers:: Integer -> Integer -> Integer
addTwoNumbers x y = x + y

square:: Integer -> Integer
square x = x * x

-- result :: Int
-- result = square $ addTwoNumbers 3 4

-- result :: Int
-- result = square (addTwoNumbers 3 4)

-- Function composition
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

composedFunction1 :: Int -> Int
composedFunction1 = double . increment

composedFunction2


