-- LTS.hs
module LTS where

import Data.List (nub, sort)
import Control.Exception (try, SomeException, evaluate)
import System.IO.Unsafe (unsafePerformIO)

-- Types
type State = Integer
type Label = String
type LabeledTransition = (State, Label, State)
type Trace = [Label]
type LTS = ([State], [Label], [LabeledTransition], State)
type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)

-- Symbols
tau :: Label
tau = "tau"

delta :: Label
delta = "delta"

-- Converts an LTS into an IOLTS
createIOLTS :: [LabeledTransition] -> IOLTS
createIOLTS transitions = (states, inputs, outputs, transitionsSet, initState)
  where
    (states, labels, transitionsSet, initState) = createLTS transitions
    inputs = [ tail l | l <- labels, head l == '?' ]
    outputs = [ tail l | l <- labels, head l == '!' ]

-- Creates an LTS from a list of transitions
createLTS :: [LabeledTransition] -> LTS
createLTS transitions = (states, labels, transitionsSet, initState)
  where
    states = makeSet (concatMap (\(from, _, to) -> [from, to]) transitions)
    labels = makeSet [ l | (_, l, _) <- transitions, l /= tau ]
    transitionsSet = transitions
    initState = head states

makeSet :: Ord a => [a] -> [a]
makeSet = sort . nub

-- Function to generate all possible input traces up to a certain depth
generateInputTraces :: IOLTS -> Int -> [Trace]
generateInputTraces iolts@(states, inputs, outputs, transitions, initState) depth =
    nub $ generateInputTracesFromState iolts initState [] depth

generateInputTracesFromState :: IOLTS -> State -> Trace -> Int -> [Trace]
generateInputTracesFromState _ _ trace 0 = [trace]
generateInputTracesFromState iolts@(states, inputs, outputs, transitions, initState) currentState currentTrace depth =
    let
        inputTransitions = [ (tail l, t) | (s, l, t) <- transitions, s == currentState, head l == '?' ]
        nextTraces = concat [ generateInputTracesFromState iolts t (currentTrace ++ [l]) (depth - 1)
                             | (l, t) <- inputTransitions ]
    in
        if null inputTransitions
        then [currentTrace]
        else nextTraces

-- Simulate the SUT along a given trace
simulateSUT :: (State -> Label -> (State, Label)) -> State -> Trace -> [(State, Label, Label)]
simulateSUT _ _ [] = []
simulateSUT sut currentState (inputLabel:restInputs) =
    let (newState, outputLabel) = sut currentState inputLabel
        restSimulation = simulateSUT sut newState restInputs
    in (currentState, inputLabel, outputLabel) : restSimulation

-- Function to get the possible output labels from a given state in the IOLTS
outputLabelsFromState :: IOLTS -> State -> [Label]
outputLabelsFromState (_, _, _, transitions, _) currentState =
    [ tail l | (s, l, _) <- transitions, s == currentState, head l == '!' ]

-- Function to get the transitions from the IOLTS
getTransitions :: IOLTS -> [LabeledTransition]
getTransitions (_, _, _, transitions, _) = transitions

-- Main testing function
testLTSAgainstSUT :: IOLTS -> (State -> Label -> (State, Label)) -> Either [String] Bool
testLTSAgainstSUT iolts sut =
    let
        maxDepth = 10 -- Increased depth to capture longer traces
        inputTraces = generateInputTraces iolts maxDepth
        testResults = map (testTrace iolts sut) inputTraces
        errors = [ err | Left err <- testResults ]
    in
        if null errors
        then Right True
        else Left errors

-- Function to test a single trace
testTrace :: IOLTS -> (State -> Label -> (State, Label)) -> Trace -> Either String Bool
testTrace iolts sut trace =
    checkTrace iolts sut (getInitialState iolts) trace []
  where
    getInitialState (_, _, _, _, initState) = initState

-- Recursive function to check the trace step by step
checkTrace :: IOLTS -> (State -> Label -> (State, Label)) -> State -> Trace -> Trace -> Either String Bool
checkTrace _ _ _ [] _ = Right True
checkTrace iolts sut sutState (inputLabel:restInputs) inputsSoFar =
    let
        -- Find next IOLTS states after input
        nextIoltsStatesAfterInput = concatMap (\state -> [ t | (s, l, t) <- getTransitions iolts, s == state, l == ('?':inputLabel) ]) [sutState]
    in if null nextIoltsStatesAfterInput
       then Left ("Input '" ++ inputLabel ++ "' is not allowed in the model after trace " ++ show inputsSoFar)
       else case tryApplyInput sut sutState inputLabel of
            Left err -> Left ("At input trace " ++ show (inputsSoFar ++ [inputLabel]) ++ ", SUT raised an exception: " ++ err)
            Right (newSutState, outputLabel) ->
                let
                    -- Find possible outputs from IOLTS after input
                    possibleOutputs = concatMap (\state -> [ tail l | (s, l, _) <- getTransitions iolts, s == state, head l == '!' ]) nextIoltsStatesAfterInput
                    -- Find next IOLTS states after output
                    nextIoltsStates = concatMap (\state -> [ t | (s, l, t) <- getTransitions iolts, s == state, l == ('!':outputLabel) ]) nextIoltsStatesAfterInput
                in if null possibleOutputs
                   then Left ("No output expected after input '" ++ inputLabel ++ "', but SUT produced output '" ++ outputLabel ++ "'.")
                   else if outputLabel `elem` possibleOutputs
                        then checkTrace iolts sut newSutState restInputs (inputsSoFar ++ [inputLabel])
                        else Left ("At input trace " ++ show (inputsSoFar ++ [inputLabel]) ++ ", SUT produced output '" ++ outputLabel ++ "', which is not allowed. Allowed outputs: " ++ show possibleOutputs)

-- Function to safely apply input to SUT
tryApplyInput :: (State -> Label -> (State, Label)) -> State -> Label -> Either String (State, Label)
tryApplyInput sut state inputLabel =
    unsafePerformIO $ do
        result <- try (evaluate (sut state inputLabel)) :: IO (Either SomeException (State, Label))
        return $ case result of
            Left err -> Left (show err)
            Right val -> Right val

-- Example IOLTS model for the door
doorModel :: IOLTS
doorModel = createIOLTS [
    (0, "?close", 1),
    (1, "!closed", 1),
    (1, "?open", 0),
    (0, "!opened", 0),
    (1, "?lock", 2),
    (2, "!locked", 2),
    (2, "?unlock", 1),
    (1, "!unlocked", 1)
    ]

-- Door implementations
doorImpl1 :: State -> Label -> (State, Label)
doorImpl1 0 "close" = (1, "closed")
doorImpl1 1 "open" = (0, "opened")
doorImpl1 1 "lock" = (2, "locked")
doorImpl1 2 "unlock" = (1, "unlocked")
doorImpl1 _ _ = error "Invalid command and/or state!"

doorImpl2 :: State -> Label -> (State, Label)
doorImpl2 0 "close" = (1, "opened")  -- Bug: Incorrect output
doorImpl2 1 "open" = (0, "closed")   -- Bug: Swapped outputs
doorImpl2 1 "lock" = (2, "locked")
doorImpl2 2 "unlock" = (1, "unlocked")
doorImpl2 _ _ = error "Invalid command and/or state!"

doorImpl3 :: State -> Label -> (State, Label)
doorImpl3 0 "close" = (1, "closed")
doorImpl3 1 "open" = (0, "opened")
doorImpl3 1 "lock" = (2, "locked")
doorImpl3 2 "unlock" = (2, "unlocked")  -- Bug: Does not transition back to state 1
doorImpl3 _ _ = error "Invalid command and/or state!"

doorImpl4 :: State -> Label -> (State, Label)
doorImpl4 0 "close" = (1, "closed")
doorImpl4 1 "open" = (0, "opened")
doorImpl4 1 "unlock" = (2, "locked")  -- Bug: "unlock" is not a valid input in state 1
doorImpl4 2 "lock" = (1, "unlocked")  -- Bug: Swapped "lock" and "unlock" actions
doorImpl4 _ _ = error "Invalid command and/or state!"

doorImpl5 :: State -> Label -> (State, Label)
doorImpl5 0 "close" = (1, "closed")
doorImpl5 1 "open" = (0, "open")       -- Bug: Incorrect output label
doorImpl5 1 "lock" = (2, "locked")
doorImpl5 2 "unlock" = (1, "unlocked")
doorImpl5 _ _ = error "Invalid command and/or state!"

doorImpl6 :: State -> Label -> (State, Label)
doorImpl6 0 "close" = (1, "closed")
doorImpl6 1 "open" = (3, "opened")
doorImpl6 3 "close" = (4, "closed")
doorImpl6 4 "open" = (5, "opened")
doorImpl6 5 "close" = (6, "closed")
doorImpl6 6 "open" = error "Door is stuck!"
doorImpl6 1 "lock" = (2, "locked")
doorImpl6 4 "lock" = (2, "locked")
doorImpl6 6 "lock" = (2, "locked")
doorImpl6 2 "unlock" = (1, "unlocked")
doorImpl6 _ _ = error "Invalid command and/or state!"

doorImpl7 :: State -> Label -> (State, Label)
doorImpl7 0 "close" = (1, "closed")
doorImpl7 1 "open" = (0, "opened")
doorImpl7 1 "lock" = (2, "locked")
doorImpl7 2 "unlock" = (3, "unlocked")
doorImpl7 4 "close" = (5, "closed")
doorImpl7 3 "open" = (4, "opened")
doorImpl7 3 "lock" = (2, "locked")
doorImpl7 5 "open" = (0, "opened")
doorImpl7 5 "lock" = (6, "locked")
doorImpl7 6 "unlock" = error "Incorrect keycode!"
doorImpl7 _ _ = error "Invalid command and/or state!"

doorImpl8 :: State -> Label -> (State, Label)
doorImpl8 0 "close" = (1, "closed")
doorImpl8 1 "open" = (0, "opened")
doorImpl8 1 "lock" = (2, "locked")
doorImpl8 2 "unlock" = (3, "unlocked")
doorImpl8 4 "close" = (5, "closed")
doorImpl8 3 "open" = (4, "opened")
doorImpl8 3 "lock" = (2, "locked")
doorImpl8 5 "open" = (6, "opened")
doorImpl8 5 "lock" = (2, "locked")
doorImpl8 6 "close" = (7, "closed")
doorImpl8 7 "lock" = (2, "locked")
doorImpl8 7 "close" = (2, "closed")
doorImpl8 _ _ = error "Invalid command and/or state!"


-- LTS.hs (Add this implementation alongside existing doorImpl1 to doorImpl8)

doorImplAlternative :: State -> Label -> (State, Label)
doorImplAlternative state input
    | state == 0 && input == "close"  = (1, "closed")
    | state == 1 && input == "lock"   = (2, "locked")
    | state == 2 && input == "unlock" = (1, "unlocked")
    | state == 1 && input == "open"   = (0, "opened")
    | otherwise = error "Invalid command and/or state!"
