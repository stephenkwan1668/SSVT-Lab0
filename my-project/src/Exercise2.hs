{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant return" #-}
module Exercise2 where
import Data.List
import Test.QuickCheck
import Mutation hiding (mutators)
import MultiplicationTable (multiplicationTable, prop_tenElements, prop_firstElementIsInput, prop_linear, prop_sumIsTriangleNumberTimesInput)
import Control.Monad (replicateM)

import MultiplicationTable
import Exercise1
import Mutation (mutators)


{-
    TODO
    - [ ] main function

    -- Time Spend: 240 min

    Future to-dos:
    - [ ] Make function that will run 4000 times against mutations
    - [ ] Where we get a list of True and False -> count the True for survivors
    - [ ] However do we need to check if just all mutants are killed or just that all are covered?
    - [ ] to get a actual test result, instead of only well you get the rest...
    - [ ] Make function that will try every single combination given 2 lists of fut's and props

    ===== **Approach** =====
    1) Function that can run a mutation on a property for x amount of times & returns list
    of True | False
    2) Function that can take any mutator and any property and runs it x amount of times
    and propagates back the list / counts of True | False, and returns this as an int
    3) Function that goes trough all mutators from a list, and runs each mutation
    against each property, and put it into a sort of counter.Applicative

    ==== **Results section** =====
    
-}


-- runMutations :: Integer -> IO [Bool]
-- runMutations n = do
--     functionIns <- generate $ vectorOf (fromIntegral n) (arbitrary :: Gen [Integer])
--     result <- mapM (mutate' shuffleOutput [prop_tenElements] multiplicationTable 2) functionIns
--     return result
generateNums :: Int -> IO [Integer]
generateNums n = generate $ vectorOf n (arbitrary :: Gen Integer)

-- Discuss how to deal with empty lists - aka output did not change
-- To ensure we get 

-- TODO Redundant 
mutateSkipEmpty :: Eq a => Integer -> (a -> Gen a) -> [a -> Integer -> Bool] -> (Integer -> a) -> Gen [Bool]
mutateSkipEmpty num mut props fut = mutate' mut props fut num 


-- Not very optimal
runMutations :: Eq a => Int -> (a -> Gen a) -> [a -> Integer -> Bool] -> (Integer -> a) -> IO [[Bool]]
runMutations n mut props fut = do
    nums <- generateNums n
    result <- mapM (\num -> generate $ mutateSkipEmpty num mut props fut ) nums
    return result

-- This function will check for any False in the list.
-- If one of the elements is indeed False it will be seen as killed mutant.
-- sometimes an empty list gets back indicating that the output is the same, so a weak mutant
-- we classify those as 'Killed' automatically because it can be seen as an "invalid" mutant that 
-- cannot be killed
-- does not matter if something for the same set of props survived
reverseMap :: [Bool] -> Bool
reverseMap [] = True
reverseMap xs = any not xs

-- Utility function that maps a Bool to a Int
-- It maps: True->0 and False->1
boolToInt :: Bool -> Integer
boolToInt x = if x then 0 else 1

mapList :: [[Bool]] -> [Integer]
mapList = map (boolToInt . reverseMap)

-- Combining our group-defined functions with the ones given by Mutation.hs
mutatorsFull = mutators ++ [replaceElement, changeSigOfElement, shuffleOutput]

-- Helper function that counts the number of survivors 
countSingleMutantSurvivors n mut props fut = do
    counts <- mapList <$> runMutations n mut props fut
    return (sum counts)

properties = [prop_tenElements, prop_firstElementIsInput, prop_linear, prop_sumIsTriangleNumberTimesInput]


-- I did not create this function signature my linter did :)
countSurvivors :: (Traversable t, Integral a1, Eq a2) => a1 -> t (a2 -> Gen a2) -> [a2 -> Integer -> Bool] -> (Integer -> a2) -> IO (t Integer)
countSurvivors n muts props fut = do
    counts <- mapM (\mut -> countSingleMutantSurvivors (fromIntegral n) mut props fut) muts
    return counts