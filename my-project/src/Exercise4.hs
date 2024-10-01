{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Exercise4 where
import Data.List
import Test.QuickCheck
import Mutation hiding (mutators)
import MultiplicationTable (multiplicationTable, prop_tenElements, prop_firstElementIsInput, prop_linear, prop_sumIsTriangleNumberTimesInput, prop_moduloIsZero)
import Control.Monad (replicateM)
import MultiplicationTable
import Exercise1
import Mutation (mutators)
import Exercise2
import Exercise3
import Data.Ord


{-
     Time Spend: 60 min

     ===== **Problem definition** =====

     ===== **Approach** =====
     We can take the approach of getting all mutations corresponding with 
     the mutants they killed. Then we can calculate the percentage of each
     property, how many each prop kills.


-}

-- Implementation is quite easy

-- We first get to total length (depth) of the data table as a divider
totalOfDataTable :: [[Bool]] -> Int
totalOfDataTable data_table = length data_table

-- Then for a list of bools we count how many of the entries were false
falseCount :: [Bool] -> Integer
falseCount row = fromIntegral $ length (filter (== False) row)

-- Then we transpose the datable so we have the columns as rows -> then we can feed it through falseCount
-- Then we map falseCount on each row so we get the same property list but then with the numbers they killed
getKillCountPerProp :: [[Bool]] -> [Integer]
getKillCountPerProp dataTable = map falseCount (transpose dataTable)

-- To get the percentage we use list comprehension to apply the percentage calculation on each entry of the kill count
getPercentage :: Integer -> [Integer] -> [Double]
getPercentage total_mutants number_of_kills = [fromIntegral x / fromIntegral total_mutants * 100 | x <- number_of_kills] 

getTotalMutants :: [[Bool]] -> Int
getTotalMutants dataTable = totalOfDataTable dataTable

-- The actual program
-- We get collect the data table, calculate the mutants and kills and get the percentages in a list corresponding 
-- with each property
result :: IO ()
result = do
     dataTable <- collectAllKills 4000 multiplicationTableProps
     let totalMutants = getTotalMutants dataTable
     let number_of_kills = getKillCountPerProp dataTable
     print $ getPercentage (fromIntegral totalMutants) number_of_kills

