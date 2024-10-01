module Exercise6 where

import Exercise1
import Exercise2
import Exercise3 (collectAllKills, main_Ex3)
import Exercise4
-- import Exercise4 hiding (main)
-- import Exercise5

import MultiplicationTable (multiplicationTableProps)
import Mutation

import Test.QuickCheck
import Data.List
import Control.Monad

{-
     Time Spend: 180 min

     ===== **Main goals** =====
     - Visualisation of previous exercises

     - Percentage
     - Redundancy -> minimal property subset
     - Coverage ?

     Proposal
-}

-- visualiseMutationResults :: (Eq a) => (Integer -> a) -> [a -> Integer -> Bool] -> Int -> IO ()
-- visualiseMutationResults fut props numMutations = do
--     -- Generate the mutations
--     mutations <- generate $ vectorOf numMutations (arbitrary :: Gen (a -> Gen a))

--     -- Run the mutations and collect results
--     results <- mapM (\mut -> runMutations numMutations mut props fut) mutations

--     -- Count survivors for each mutation
--     survivorCounts <- mapM (\mutResults -> sum . map (boolToInt . reverseMap) $ mutResults) mutations

--     -- Print the results in a table format
--     putStrLn "Mutation Results:"
--     putStrLn "-----------------------------------------"
--     putStrLn "Mutation | Survivors | Killed"
--     putStrLn "-----------------------------------------"
--     mapM_ (\(mut, survivors) -> putStrLn $ show mut ++ " | " ++ show survivors ++ " | " ++ show (numMutations - survivors)) (zip mutations survivorCounts)
--     putStrLn "-----------------------------------------"

-- main :: IO ()
-- main = do
--     let fut = multiplicationTable
--         props = [prop_tenElements, prop_firstElementIsInput, prop_linear, prop_sumIsTriangleNumberTimesInput, prop_moduloIsZero]
--         numMutations = 4000

--     visualiseMutationResults fut props numMutations


-- -- Function to create a grid of elements with separators
-- grid :: [[a]] -> String
-- grid elements = intercalate "\n--------\n" $ map (intercalate " | ") (transpose elements)

-- -- Example usage
-- testPrint :: IO ()
-- testPrint = do
--     let data = [["Name", "Age", "City"],["Alice", "25", "New York"],["Bob", "30", "Los Angeles"],["Charlie", "35", "Chicago"]]

--     putStrLn $ grid data

-- length = 42
headSeparator :: String
headSeparator = "==============="

exHead :: String -> String
exHead ex = (headSeparator ++ " " ++ ex ++ " " ++ headSeparator ++ "\n")

separatorLine :: String
separatorLine = ((replicate 42 '-') ++ "\n")

-- Function to format a double with a specific number of decimal places
roundTo :: Int -> Double -> Double
roundTo n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)

-- Exercise 1
listOfPropsEx1 :: String
listOfPropsEx1 = "** 1. Random element change **\n\n"
     <> "From all the mutations, this is the strongest because it introducesa a subtle change that might be missed by a lot\n"
     <> "of properties - especially by the properties that focus on the first element on very specific values. It keeps the list\n"
     <> "length and order intact, hence why it makes it more imperceptible and susceptible to be missed.\n"
     <> "\n"
     <> "** 2. Add `n` to each element in the list **\n\n"
     <> "Similar to the previous one, this one is quite strong because the degree of change is subtle and the specificity\n"
     <> "quite high. If the list is ordered, it becomes potentially even stronger because it won't change this property.\n"
     <> "This operation is quite useful to detect bugs that are related with \"parity\" (odd / even) or other basic arithmetic\n"
     <> "opeations. Since it changes a specific amount (which can be arbitrarly small), it might go undetected by properties\n"
     <> "that don't check this. The value `1` is just for the sake of simplicity, it can be any arbitrary number. Of course,\n"
     <> "it has to be `(>0)`.\n"
     <> "\n"
     <> "** 3. Random shuffle **\n\n"
     <> "This type of mutation becomes weaker as it is broader and less specific when it comes to identify logical errors; it is\n"
     <> "still quite practical to detect \"order\" though, we would figure. With a big enough list, it becomes quite likely - even\n"
     <> "though it is not absolutely guaranteed -, that the list will not be ordered anymore. Possibly, one future idea would be\n"
     <> "to change a number that is context-aware: meaning that it will change depending on the previous / next item. This is\n"
     <> "described in the discussion section.\n"
     <> "\n"
     <> "** 4. Sign flip **\n\n"
     <> "From all mutations, this one is the weakest because it overlaps with the element replacement mutation. Even though it\n"
     <> "checks specifically for negative numbers (which can be interesting), it can also be redundant if the number Generator\n"
     <> "also includes negative numbers. Here we wanted to separate concerns and being aware of this, we decided that it would\n"
     <> "be more interesting to observe if there are any clear patterns arising from its separate usage.\n"
     <> "\n"



-- Exercise 2 + 3
nrMutations :: Int
nrMutations = 4000
-- allKills :: IO [[Bool]]
-- allKills = collectAllKills 4000 multiplicationTableProps

totalMuts :: [[Bool]] -> Int
totalMuts dataTable = (length $ head dataTable) * (length dataTable)


-- Exercise 5
     -- Create a visualization function that takes the FUT, properties, and number of mutants
visualizeMutationResults :: (Integer -> [Integer]) -> [[Integer] -> Integer -> Bool] -> Int -> IO ()
visualizeMutationResults fut props numMutants = do
    -- Run mutation tests with 4000 mutants
    let allMutations = mutators++ex1Mutators
    mutationResults <- mapM (\mut -> runMutations numMutants mut props fut) allMutations
    
    -- Filter out empty results (equivalent mutants) and concatenate lists per mutator
    let killedMutants = concatMap (filter (not . null)) mutationResults
    
    let transposedMutants = transpose killedMutants
    
    putStrLn "=== Visualization of Mutation Testing ==="
    putStrLn "Each row represents a property, each column a mutant. 'T' means the mutant was killed."
    let visualData = map (map (\b -> if b then 'T' else 'F')) transposedMutants
    mapM_ (putStrLn . concatMap (\c -> [c, ' '])) visualData

    -- Additional statistics or summary can go here, like how many mutants each property killed.
    putStrLn "\nSummary of killed mutants per property:"
    mapM_ (print . length . filter id) transposedMutants
    -- putStrLn $ exHead "Exercise 5"
    -- putStrLn separatorLine
