module Main where

import qualified Data.Text.IO       as TI
import           System.Environment (getArgs)

import           Lib                (golangToHtml)

main :: IO String
main = do
  args <- getArgs
  case args of
    [] -> return "Usage: ./golangToHtml <filename>"
    _ -> do
      code <- TI.readFile $ args !! 0
      TI.writeFile "source.html" (golangToHtml code)
      return "Source written."
