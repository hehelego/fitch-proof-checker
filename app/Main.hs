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
        then showProof proof >> check proof
        else putStrLn "Cannot parse the entire file:" >> putStrLn unparsed

showProof :: Proof -> IO ()
showProof (Proof steps) = putStrLn "BEGIN PROOF" >> mapM_ print steps >> putStrLn "END PROOF"

check :: Proof -> IO ()
check proof =
  if checkProofSyntax proof
    then
      let (mbck, log) = runWriter $ runExceptT $ checkProof proof
       in case mbck of
            Right _ -> putStrLn "Correct proof" >> putStrLn log
            Left err -> putStr "Incorrect proof: " >> print err
    else putStrLn "Invalid proof syntax"
