module Main where

import Exercise1
import Exercise2 
import Exercise3 (collectAllKills, main_Ex3)
import Exercise4
import Exercise5
import Exercise6

import MultiplicationTable (multiplicationTableProps)
import Mutation

import Test.QuickCheck
import Data.List
import Control.Monad

main :: IO()
main = do

     -- Ex3 and 4
     allKills <- collectAllKills nrMutations multiplicationTableProps
     -- Ex 4
     let totalMutants = getTotalMutants allKills
     let numberOfKills = getKillCountPerProp allKills

     -- Exercise 1
     putStrLn $ exHead "Exercise 1"
     putStrLn "Properties that were defined in addition to the ones provided in the workshop"
     putStrLn "with a brief description about their strenghts/weaknesses:\n"

     putStrLn listOfPropsEx1
     putStrLn separatorLine
     -- Exercise 2
     putStrLn $ exHead "Exercise 2"
     putStrLn "Description Exercise 2"
     putStrLn separatorLine

     -- TODO print all kills and no kills? with percentage?
     -- That's already on another exercise?
     -- Exercise 3
     putStrLn $ exHead "Exercise 3"
     putStrLn "** Task **"
     putStrLn "Generate the minimal property subset from a set of properties.\n"
     putStrLn ("Generating all subsets for " ++ (show nrMutations) ++ " mutations\n")
     putStrLn ("... for a total of " ++ (show $ length $ head allKills) ++ " subsets")
     putStrLn ("From a total of " ++ show (totalMuts allKills) ++ " mutations,")
     print $ length allKills

     putStrLn separatorLine

     -- Exercise 4
     putStrLn $ exHead "Exercise 4"
     putStrLn "** Task **"
     putStrLn "For the mutants generated, calculate the percentage of those which were killed."

     putStrLn ("Total number of mutants: " ++ show totalMutants)

     let decimalPlaces = 3
     let percentages = getPercentage (fromIntegral totalMutants) numberOfKills
     let roundPercentages = map (\value -> roundTo decimalPlaces value) percentages
     mapM_ (\value -> print ((show value) ++ "%")) roundPercentages

     putStrLn separatorLine

     -- -- Exercise 5
     -- putStrLn $ exHead "Exercise 5"
     -- putStrLn separatorLine

