module Exercise5 where

import LTS

-- IOLTS model for doorImpl1
-- doorImpl1Transitions :: [LabeledTransition]
-- doorImpl1Transitions = [
--     (0,"?close",1),(0,"!closed",0),
--     (1,"?open",0),(1,"!opened",1),
--     (1,"?lock",2),(1,"!locked",1),
--     (2,"?unlock",1),(2,"!unlocked",2)
--     ]

-- doorImpl1Transitions :: [LabeledTransition]
-- doorImpl1Transitions = [
--     (0,"?close",1),
--     (1,"?open",0),
--     (1,"?lock",2),
--     (2,"?unlock",1)
--     ]

-- doorImpl1Model :: IOLTS
-- doorImpl1Model = createIOLTS doorImpl1Transitions


-- doorImpl1ModelLTS :: LTS
-- doorImpl1ModelLTS = createLTS doorImpl1Transitions

-- -- Function to generate traces from the IOLTS
-- generateTracesFromState :: IOLTS -> State -> [Label] -> Int -> [[Label]]
-- generateTracesFromState (states, _, _, transitions, _) state trace depth
--     | depth <= 0 = [reverse trace]
--     | otherwise =
--         let nextTransitions = [ (label, nextState) | (fromState, label, nextState) <- transitions, fromState == state ]
--         in if null nextTransitions
--            then [reverse trace]
--            else concat [ generateTracesFromState (states, [], [], transitions, 0) nextState (label:trace) (depth - 1) | (label, nextState) <- nextTransitions ]

-- generateTraces :: IOLTS -> Int -> [[Label]]
-- generateTraces iolts@(_, _, _, _, initState) maxDepth =
--     generateTracesFromState iolts initState [] maxDepth

-- -- The test function
-- testLTSAgainstSUT :: IOLTS -> (State -> Label -> (State, Label)) -> Bool
-- testLTSAgainstSUT iolts@(states, inputLabels, outputLabels, transitions, initState) sut =
--     all (testTrace initSUTState) tracesList
--     where
--         tracesList = generateTraces iolts 15
--         initSUTState = 0 -- Initial state of the SUT

--         testTrace :: State -> [Label] -> Bool
--         testTrace sutState [] = True
--         testTrace sutState (label:labels)
--             | head label == '?' =
--                 let inputLabel = tail label
--                     (newState, outputLabel) = sut sutState inputLabel
--                 in if not (null labels) && head labels == ('!' : outputLabel) then
--                        testTrace newState (tail labels)
--                    else if not (null labels) then
--                        error ("Mismatch at state " ++ show sutState ++ ": expected output '" ++ tail (head labels) ++ "', got '" ++ outputLabel ++ "'")
--                    else
--                        True
--             | otherwise = testTrace sutState labels
