{-
    DO NOT SUBMIT / REVIEW!!!
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Exercise3 where

import Exercise1
import Exercise2

import MultiplicationTable
import Mutation

import Data.List
import Data.Ord
import Test.QuickCheck
import Control.Monad (replicateM)

{-

     TODO: 
     - [ ] "ELABORATE THIS FUNCTION A BIT MORE"?
     - [ ] According to the lecture, what defines a minimal property subset is: properties that do not kill mutants are
          deemed 'redundant' and the ones that share the same mutants' killing. Since I (@goncalo) cannot test this, but
          I remember that this could be something useful. If not, at least let's put this into the discussion.
     - [ ] Double check functions and refactor the comments. I (@goncalo) did not change a lot because I was not sure
          what was "final" and what was still WIP.

     Time Spend: 240 min
     
     ===== **Problem statement** =====
     Find the minimal property subset with a 'function under test" and a set of properties.

     ===== **Discussion** =====
     Cover set problem -> NP-hard problem
     Can be solved with brute-force, but when number of props increase can be ??????
     Greedy algorithms can approximate it, in this implementation chosen for brute-force -> just because of ease of
     implementation and not to many props so the amount of combinations are manageable 

     ===== **Data collection** =====
     Because of the way that I implemented parts of exercise 2, I can get the data back per mutation 
     as a list of lists where each row indicates a specific mutation and each column with the corresponding property
     Which is True or False, False meaning that the specific property killed that specific mutation. And true means it survived
     To see if certain combinations of properties kills all mutations we can combine all the mutations from all the mutors in one 
     big data table. Which will look a bit like this: 

     | Prop1 | Prop2 | Prop3 |
     |-------|-------|-------|
     | False | True  | False | -> a result of the prop tested on a arbitrary muted list
     | True  | True  | False | -> ... 
     | False | False | True  | -> ...

     Note: Sometimes a mutation does not change the output and we get back an empty list - this because
     mutate' returns an empty list in that case. When creating the data table we remove those

-}

-- Gather all mutation results per property - and remove all  empty lists if they exist
collectAllKills :: Int -> ([[Integer] -> Integer -> Bool]) -> IO [[Bool]]
collectAllKills n props = do
     res <- mapM (\mut -> runMutations n mut props multiplicationTable) mutatorsFull
     -- Filtering the empty lists
     let finalRes = filter (not . null) (concat res)
     return finalRes

-- From the data table above we need to find the minimal subset of properties that kill all mutants
-- which can be understood as that every row contains at least one false (killed) for the least amount of properties 
-- representing that row

-- https://en.wikipedia.org/wiki/Set_cover_problem
-- After some head breaking, found out that this is exactly a set cover problem -> which minimum number of sets covers all
-- Which is classified as NP-hard. Meaning it can be easily verified if a solution has been found. However finding the minimal solution
-- does not have a best approach. Only with brute-force however the complexity grows very fast if the number of props increase.
-- Namely exponential complexity O(2^n) at which some point brute force is not feasible smallest solution. 
-- A different approach is using a greedy algorithm, however this might not return the smallest subset, but can handle large numbers
-- of properties. In my approach I decided to go for brute force, one because the number of properties is not that large 
-- -> number_props = 5  powerset = P(2^n) thus number of sets = 2^5 = 32 possible combinations. Secondly, I tried to start 
-- with implementing a greedy algorithm but to no avail, and was taking a lot of time to understand the implementation in haskell
-- There was a library that did exactly what i wanted but it was sadly outdated and incompatible

-- This is an helper function for brute forcing
-- it checks for any given combination of props (columns idx) and corresponding row (mutations) 
-- And checks if at least one of the props got killed, and returns true if so. ELABORATE THIS FUNCTION A BIT MORE
killsMutants :: [Int] -> [[Bool]] -> Bool
killsMutants selectedProps dataTable = all (\row -> any (\prop -> not (row !! prop)) selectedProps) dataTable

getNumProperties :: [[Bool]] -> Int
getNumProperties dataTable = length (head dataTable) -- Gets the first row and the length of it

getAllSubsets :: Int -> [[Int]]
getAllSubsets numProperties = subsequences [0..numProperties - 1]

-- This will firstly generate all possible subsets that are possible to make from the given props 
-- Then filters all subsets and leaving only the subsets that kill all mutants (defined by killsMutants)
-- This list is then filtered by comparing the lenght and returning the subset that has the minimum amount of props.
-- Because all combinations are made this is classified as brute-force
minKillingSubset :: [[Bool]] -> [Int]
minKillingSubset dataTable =
    let numProperties = getNumProperties dataTable
        allSubsets = getAllSubsets numProperties
        validSets = filter (\subset -> killsMutants subset dataTable) allSubsets
         in minimumBy (comparing length) validSets


-- For Exercise6
main_Ex3 :: IO()
main_Ex3 = main

main :: IO ()
main = do
     allKills <- collectAllKills 4000 multiplicationTableProps
     print $ length allKills
     print $ minKillingSubset allKills


-- Results
-- When we run the function above we see that smallest possible subset is property 0 and 3
-- We can verify this result by running the countSurvivors with only property 0 (prop_tenElements)  and 3 (prop_linear)
-- countSurvivors 4000 mutatorsFull [prop_tenElements, prop_linear] multiplicationTable
-- This results in 0 surviving mutants indicating that these properties indeed cover all mutations.