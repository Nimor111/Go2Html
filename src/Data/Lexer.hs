{-# LANGUAGE OverloadedStrings #-}

module Data.Lexer where

import           Data.Char       (isAlpha, isAlphaNum)
import           Data.Map.Strict as MapS
import           Data.Maps       (keywords, operators, predeclIdentifiers)
import qualified Data.Text       as T
import           Data.Token
import           Data.TokenTypes
import           Debug.Trace

safeHead :: T.Text -> Maybe Char
safeHead s =
  if s == ""
    then Nothing
    else Just $ T.head s

safeTail :: T.Text -> Maybe T.Text
safeTail s =
  if s == ""
    then Nothing
    else Just $ T.tail s

token :: MapS.Map T.Text TokenType -> T.Text -> Line -> Column -> Maybe Token
token m ident line column =
  Just $
  Token
    {tokenType = (MapS.!) m ident, lexeme = ident, location = (line, column)}

parseIdent :: T.Text -> Line -> Column -> Maybe Token
parseIdent ident line column
  | MapS.member ident keywords = token keywords ident line column
  | MapS.member ident predeclIdentifiers =
    token predeclIdentifiers ident line column
  | otherwise =
    Just $
    Token
      { tokenType = Ident (Identifier ident)
      , lexeme = ident
      , location = (line, column)
      }

hasNewLine :: T.Text -> Bool
hasNewLine "" = False
hasNewLine s =
  if T.head s == '\n'
    then True
    else False

tokenize :: T.Text -> Line -> Column -> [Maybe Token]
-- tokenize code
--   | trace ("Code is: " <> (T.unpack code)) False = undefined
tokenize code line column =
  case safeHead code of
    Nothing -> [Nothing]
    Just c ->
      case isAlpha c of
        True ->
          let ident = T.takeWhile (\x -> x /= ' ' && x /= '\n') code
              rest = T.dropWhile (\x -> x /= ' ' && x /= '\n') code
              newLine =
                if hasNewLine rest
                  then (line + 1, 0)
                  else (line, column + T.length ident)
           in parseIdent (T.strip ident) line (column + T.length ident) :
              tokenize (T.strip rest) (fst newLine) (snd newLine)
        False -> [Nothing]
