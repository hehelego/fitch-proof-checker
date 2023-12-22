module Main where

import Check
import Control.Monad.Except
import Control.Monad.Writer
import FitchParse (Parser (runParser), proofP)
import Proof
import Prop
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let proofFile = head args
  proofText <- readFile proofFile
  case runParser proofP proofText of
    Left err -> print err
    Right (proof, unparsed) ->
      if null unparsed
        then showPF proof >> checkPF proof
        else putStrLn "Cannot parse the entire file:" >> putStrLn unparsed

showPF :: Proof -> IO ()
showPF = mapM_ print

checkPF :: Proof -> IO ()
checkPF proof =
  putStrLn log >> case ck of
    Right _ -> putStrLn "Correct"
    Left err -> putStr "Wrong" >> putStrLn err
  where
    run = runWriter . runExceptT
    (ck, log) = run $ checkProofSyntax proof >> checkProof proof
