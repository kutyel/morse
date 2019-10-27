module Main where

import           Control.Monad      (forever, when)
import           Morse              (morseToChar, stringToMorse)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO

convertLine :: (String -> IO ()) -> IO ()
convertLine convert = do
  weAreDone <- isEOF
  when weAreDone exitSuccess
  -- otherwise, proceed.
  line <- getLine
  convert line

convertToMorse :: IO ()
convertToMorse = forever $ convertLine to
  where
    to line =
      case stringToMorse line of
        (Just str) -> putStrLn (unwords str)
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ convertLine from
  where
    from line =
      case traverse morseToChar (words line) of
        (Just s) -> putStrLn s
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] ->
      case arg of
        "from" -> convertFromMorse
        "to"   -> convertToMorse
        _      -> argError
    _ -> argError
  where
    argError = do
      putStrLn
        "Please specify the first argument as being\
              \ 'from' or 'to' morse, such as: morse to"
      exitFailure
