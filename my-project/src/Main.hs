-- Main.hs
module Main where

import LTS
import Control.Exception (try, SomeException, evaluate)

main :: IO ()
main = do
    let doorImplementations = [ ("doorImpl1", doorImpl1)
                              , ("doorImpl2", doorImpl2)
                              , ("doorImpl3", doorImpl3)
                              , ("doorImpl4", doorImpl4)
                              , ("doorImpl5", doorImpl5)
                              , ("doorImpl6", doorImpl6)
                              , ("doorImpl7", doorImpl7)
                              , ("doorImpl8", doorImpl8)
                              ]
    mapM_ testDoor doorImplementations

  where
    -- Helper function to test a single door implementation
    testDoor :: (String, State -> Label -> (State, Label)) -> IO ()
    testDoor (name, impl) = do
        putStrLn $ "Testing " ++ name ++ ":"
        result <- try (evaluate (testLTSAgainstSUT doorModel impl)) :: IO (Either SomeException (Either [String] Bool))
        case result of
            Right (Right True)  -> putStrLn "Pass\n"
            Right (Left errs)   -> do
                putStrLn "Fail:"
                mapM_ (putStrLn . ("  " ++)) errs
                putStrLn ""
            Right (Right False) -> putStrLn "Fail: Unexpected failure.\n"
            Left err            -> do
                putStrLn $ "Fail: " ++ show err ++ "\n"
