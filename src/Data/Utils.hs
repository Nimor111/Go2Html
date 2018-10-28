{-# LANGUAGE OverloadedStrings #-}

module Data.Utils where

import           Data.Char       (isAlpha, isAlphaNum)
import qualified Data.Map.Strict as MapS
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

tokenNumber :: T.Text -> Line -> Column -> Maybe Token
tokenNumber num line column =
  Just $
  Token
    { tokenType = Literal (Number num)
    , lexeme = num
    , location = (line, column + T.length num)
    }

tokenStringLiteral :: T.Text -> Line -> Column -> Maybe Token
tokenStringLiteral str line column =
  Just $
  Token
    { tokenType = Literal (StringLiteral str)
    , lexeme = str
    , location = (line, column + T.length str)
    }

tokenSpace :: T.Text -> Line -> Column -> Maybe Token
tokenSpace str line column
  | str == " " =
    Just $
    Token
      { tokenType = Space Whitespace
      , lexeme = "&nbsp;"
      , location = (line, column + 1)
      }
  | str == "\n" =
    Just $
    Token {tokenType = Space NewLine, lexeme = "<br>", location = (line + 1, 0)}

hasNewLine :: T.Text -> Bool
hasNewLine "" = False
hasNewLine s =
  if T.head s == '\n'
    then True
    else False

stripPrefix :: T.Text -> T.Text
stripPrefix = T.dropWhile (\x -> x /= ' ' && x /= '\n' && (not $ isOperator x))

getPrefix :: T.Text -> T.Text
getPrefix = T.takeWhile (\x -> x /= ' ' && x /= '\n' && (not $ isOperator x))

countSpacesInFront :: T.Text -> Int
countSpacesInFront "" = 0
countSpacesInFront s
  | isAlpha $ T.head s = 0
  | T.head s == ' ' = 1 + (countSpacesInFront $ T.tail s)
  | otherwise = countSpacesInFront $ T.tail s

isOperator :: Char -> Bool
isOperator c = any (== c) (map (T.head . T.pack . show) [Plus ..])

isOnePlaceOperator :: Char -> Bool
isOnePlaceOperator c =
  c `elem`
  map (T.head . T.pack . show) (filter (\x -> (length $ show x) == 1) [Plus ..])

isWhitespace :: Char -> Bool
isWhitespace c = [c] == " "

isNewLine :: Char -> Bool
isNewLine c = [c] == "\n"

isTwoPlaceOperator :: T.Text -> Bool
isTwoPlaceOperator s =
  s `elem` map (T.pack . show) (filter (\x -> (length $ show x) == 2) [Plus ..])

getDecNumber :: T.Text -> T.Text
getDecNumber code = T.takeWhile isAlphaNum code

getStringLiteral :: T.Text -> T.Text
getStringLiteral code = "\"" <> T.takeWhile (/= '"') code <> "\""
