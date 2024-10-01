module Exercise5 where

import Data.List
import qualified Exercise2 as Ex2
import MultiplicationTable
import SetOrd
import Test.QuickCheck
import Data.Function
import Data.Foldable
import qualified Data.Bifunctor as Bifunctor

-- Time Spent: 360 min

-- First let us define a type alias for prop
type Prop a = a -> Integer -> Bool

-- Those functions will help us to formulate property sets.
-- This one will take two properties P and Q, and produce another prop R that holds if and only if P && Q hold.
cnj :: (Eq a) => Prop a -> Prop a -> a -> Integer -> Bool
cnj p q x y = p x y && q x y

-- This function generates a property set from a list of properties.
cnj' :: (Eq a) => [Prop a] -> Prop a
cnj' = foldr cnj (\_ _ -> True)

{-
  Now, we want to test all possible property sets of multiplication table properties.
  To do this, we will create a 'powerset' of properties, without the empty set.
  I.e : 1 2 3 -> 1, 2, 3, 1 && 2, 1 && 3, 2 && 3, 1 && 2 && 3
-}
propCombinations :: [Prop [Integer]]
propCombinations = map cnj' $ filter (not . null) (subsequences multiplicationTableProps)

-- And we will create a similar 'powerset' containing all property names.
propNameCombinations :: [[Int]]
propNameCombinations = filter (not . null) (subsequences [1 .. length multiplicationTableProps])

{-
  We create a function 'assocs' to to parse a list of tuples into an associative array.
  I.e if we pass a list of [(1,2),(2,3),(1,3),(2,5)(5,6)] we get back
    [(1,[2,3]),(2,[3,5]),(5,[6])]
  This will help us later, when we want to group property sets into equivalence classes.
  Using a hashmap would be way more efficient, but it requires defining our own hash function.
  We decided we would rather leave this out of scope for this exercise.
-}
addAssocs :: (Eq a) => [(a, [b])] -> (a, b) -> [(a, [b])]
addAssocs [] (k2,v2) = [(k2,[v2])]
addAssocs ((k1,v1):rest) (k2,v2)
  | k1 == k2 = (k1,v2:v1) : rest
  | otherwise = (k1,v1) : addAssocs rest (k2,v2)

assocs :: (Eq a) => [(a, b)] -> [(a, [b])]
assocs = foldl addAssocs []

main = do
  -- First, let's run 4000 mutations per mutator on all of the property sets we created earlier.
  res <- mapM (\mut -> Ex2.runMutations 4000 mut propCombinations multiplicationTable) Ex2.mutatorsFull

  {- 
    Now, we can filter out the empty lists (equivalent mutants) and concat the lists per mutator, to get
    a data table in the form of:
           Prop1   Prop2   Prop3   
         [[T,      T,      F    ...]  Mutant1
          [T,      T,      F    ...]  Mutant2
          [F,      F,      T    ...]] Mutant3
  -}
  let killedMutants = concatMap (filter (not . null)) res

  {-
    Then we can just transpose the list, to get a list of killed mutants per property set.
            Mutant1  Mutant2  Mutant3
    Prop1 [[T,       T,       T,       ...]
    Prop2  [T,       T,       F,       ...]
    Prop3  [F,       F,       T,       ...]]
  -}
  let transposedMutants = transpose killedMutants

  {-
    Now let us create equivalence classess using an associative array. This means that the property sets that
    killed exactly the same mutants wil be grouped together.
  -}
  let equivalenceClasses = assocs (zip transposedMutants propNameCombinations)


  {-
    We will use equivalence classes to find two types of conjectures: equivalences and implications.
    We were inspired by the approach desribed in FitSpec: Refining Property Sets for Functional Testing, page 6.
    Below, we print them to stdout:
  -}
  putStrLn "Conjectures:"
  showEquivalences equivalenceClasses
  showImplications equivalenceClasses

{-
  To find equivalences we need to simply print out the property sets from each of the equivalence classes
  created earlier.
-}
showEquivalences :: [([Bool], [[Int]])] -> IO ()
showEquivalences classes = do
  -- Filter out equivalence classes of length 1, because those do not contain any conjectures.
  let filtered = filter (\(k,v) -> length v > 1) classes
  -- Format the conjectures nicely to get strings like {1,2,3} = {1,3}
  let conjectures = map (namesToConjectures . snd) filtered
  let equivalences = map (intercalate " = ") conjectures
  mapM_ putStrLn equivalences

showImplications :: [([Bool], [[Int]])] -> IO ()
showImplications classes = do
  {-
    We discard all of the property sets in equivalence class save for the shortest for the sake of conciseness 
    (i.e if 1 is equivalent to 1,2,3,4 and 1,4,5, then we can discard the later property sets).
  -}
  let smallest = smallestInClass classes

  {-
    Now, we want to use pairwise operation to compare all pairs of equivalence classes (we have filtered
    out all besides one set from each class, so the pairs contain only two property sets).
  -}
  let implications = map (uncurry findImplications) (pairwise smallest)
  mapM_ maybePrint implications

  -- This function finds filters out all besides the smallest property set in an equivalence class
smallestInClass :: [([Bool], [[Int]])] -> [([Bool], [Int])]
smallestInClass xs = Bifunctor.second (minimumBy (compare `on` length)) <$> xs

{-
  Pairwise iteration used to compare all of the property sets like:
  [{a},{b},{c}] -> ({a},{b}), ({a},{c}), ({b},{c}), ...
-}
pairwise :: [a] -> [(a, a)]
pairwise xs = [(x, y) | (x:ys) <- tails xs, y <- ys]
  where
    tails [] = []
    tails l@(_:xs) = l : tails xs

data Implication = Implication {
  p :: [Int],
  q :: [Int]
}

instance Show Implication where
  show (Implication p q) = showConjecture p ++ " ==> " ++ showConjecture q

{-
  Let us create a function for finding implications. This function takes two tuples of
  (killed mutants, property sets) as arguments. Then we map killed mutants lists to sets
  of integers as described in the comment below. We can now formulate the following logic branches:
    1. Any of properties from property set 1 is also in property set 2 -> No implication
    2. If property sets are equivalent -> No implication
    3. If killed set 2 is a subset of killed set 1 -> property set 1 ==> property set 2
    4. Vice versa as above
    5. Otheriwse no implication (sets are not subsets of each other)
-}
findImplications :: ([Bool], [Int]) -> ([Bool], [Int]) -> Maybe Implication
findImplications (k1,p1) (k2,p2)
    | any (`elem` p2) p1 = Nothing
    | s1 `subSet` s2 && s2 `subSet` s1 = Nothing
    | s1 `subSet` s2 = Just (Implication p1 p2)
    | s2 `subSet` s1 = Just (Implication p2 p1)
    | otherwise = Nothing
    where
      s1 = mapBoolToIntSet k1
      s2 = mapBoolToIntSet k2

{-
  In order to find an implication conjecture, we need to compare sets of killed mutants.
  To do this, we can take the list of booleans that we created earlier, and convert it
  to a set containing the numbers of all killed mutants, like so:

          Mutant1  Mutant2  Mutant3
    Prop1 [[T,       T,       T,       ...]
    Prop2  [T,       T,       F,       ...]
    Prop3  [F,       F,       T,       ...]]

    into

    Prop1 {1, 2, 3 ...}
    Prop2 {1, 2 ...}
    Prop3 {3 ...}
-}
mapBoolToIntSet :: [Bool] -> Set Int
mapBoolToIntSet xs =
  let
    zipped = zip xs [1..length xs]
    mapped = map (\(b,idx) -> if b then idx else 0) zipped
    filtered = filter (/=0) mapped
  in
  list2set filtered

{-
  Utility functions:
  formatting, printing maybe etc.
-}
namesToConjectures :: [[Int]] -> [String]
namesToConjectures = map showConjecture

showConjecture :: [Int] -> String
showConjecture xs = "{" ++ intercalate "," (map show xs) ++ "}"

maybePrint:: (Show a) => Maybe a -> IO ()
maybePrint m = for_ m print

{-
  Reflections and future work

  We have succesfully detected some apparent conjectures. 
  After comparing with FitSpec results: 
  
  conjectures: {3}   ==> {5}     96% killed (weak)
               {2,4} ==> {5}     99% killed (weak)
               {3,4} ==> {2}     99% killed (weak)

  We have detected conjectures 2 and 3.
  
  It is worth to note that the method desribed in FitSpec: Refining Property Sets for Functional Testing also 
  includes finding out the strength of the conjecutres. It is defined by the maximum percentage of the mutants 
  killed by both property sets, and given as the following interval:

  |  Weak |   Mild   |  Strong  |  Mild  |  Weak  |
  0%     11%        33%          66%    88%      00%

  This would be the next thing for us to implement.
-}