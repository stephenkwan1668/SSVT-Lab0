{-
    DO NOT SUBMIT / REVIEW!!!
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Exercise1 where
import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable (multiplicationTable, prop_tenElements, prop_firstElementIsInput, prop_linear, prop_sumIsTriangleNumberTimesInput, prop_moduloIsZero)
import Control.Monad (replicateM)

-- Time Spend: min 60 min

-- Here are 3 mutations covered that have not been covered by the given mutations
-- First: is the random change of an element to a different number
-- Instead of adding a random element we preserve the list length but with changed values
-- I would say this is a good mutation because it produces a small change, which could be overlooked
-- by certain properties such as "prop_firstElementIsInput"
-- However it does not check anything that has to do with order or list length, because that stays 
-- the same.

-- Second: I have sign flip, which will randomly change the sign of an element in the list
-- this to check if it the tests also can deal with negative numbers when they are not to be present.
-- The only additional benefit i think this give is that it checks your tests with sign flips. However
-- it can be argued that this mutation almost has the same functionality as the replace mutations

-- Third: We randomly shuffle all items in the list, keeping some property of the list but
-- such as the sum, but not keeping the order
-- This mutation is good for checking if order of output of the fut is tested, but keeping the same numbers in the list
-- so the total sum and individual numbers stay the same. However it does not take in account any changes of list lengths.

--  === Replacement Mutator === --
-- Helper Function for replaceElement
-- Inspiration from: https://www.reddit.com/r/haskell/comments/8jui5k/how_to_replace_an_element_at_an_index_in_a_list/
-- But our function needs to take in account that if we change a random number we could 
-- accidentally replace it with the same number, that would be seen as a mutant but it will survive
-- because when looking at the list it is correct.
replace :: [Integer] -> (Integer, Integer) -> [Integer]
replace [] _ = []
replace (_:xs) (0,a) = a:xs
replace (x:xs) (n,a) =
  if n < 0 && x == n
    then (x:xs)
    else x: replace xs (n-1,a)

-- Function that will choose random int and a random int in list size and return 
-- a list with a guaranteed changed element
replaceElement :: [Integer] -> Gen [Integer]
replaceElement xs = do
    replacementNum <- arbitrary :: Gen Integer
    replacementIdx <- choose (0, length xs - 1)
    return $ replace xs (fromIntegral replacementIdx, replacementNum)


--  === Signature Mutator === --
-- Helper function that changes the signature of a given location
flipSig :: [Integer] -> Integer -> [Integer]
flipSig [] _ = []
flipSig (x:xs) 0 = (-x:xs)
flipSig (x:xs) n = x: flipSig xs (n - 1)

-- Generator wrapper function that choses a random location to change the sign of a number
changeSigOfElement :: [Integer] -> Gen [Integer]
changeSigOfElement xs = do 
    replaceIdx <- choose (0, length xs - 1)
    return $ flipSig xs (fromIntegral replaceIdx)


-- === Random shuffle function === ---
-- We use the shuffle function from quickCCheck which shuffles the list.
-- We have to make sure that it does not return the same shuffled list - which is a possibility.
-- Because then it would be classified as a mutant but it will pass all the tests because in essence it 
-- is a correct output. 
-- NOTE: there is a small but non-zero chance that the shuffled list will exactly produce the same list
-- as the input list, and it will try to shuffle again and produce the exact list again, thus there exists
-- a chance that this function will run forever :)
shuffleOutput :: [Integer] -> Gen [Integer]
shuffleOutput = shuffle



