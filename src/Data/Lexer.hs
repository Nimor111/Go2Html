{-# LANGUAGE OverloadedStrings #-}

module Data.Lexer
  ( tokenize
  ) where

import           Data.Char       (isAlpha, isAlphaNum)
import qualified Data.Map.Strict as MapS
import           Data.Maps       (keywords, operators, predeclIdentifiers)
import qualified Data.Text       as T
import           Data.Token
import           Data.TokenTypes
import           Data.Utils

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

parseSingleOperator :: Char -> Line -> Column -> Maybe Token
parseSingleOperator c line column = singleOp
  where
    singleOp = token operators (T.singleton c) line (column + 1)

parseDoubleOperator :: T.Text -> Line -> Column -> Maybe Token
parseDoubleOperator op line column = doubleOp
  where
    doubleOp = token operators op line (column + 2)

tokenize :: T.Text -> Line -> Column -> [Maybe Token]
-- tokenize code
--   | trace ("Code is: " <> (T.unpack code)) False = undefined
tokenize code line column =
  case safeHead code of
    Nothing -> []
    Just c ->
      case isAlpha c || c == '_' of
        True ->
          let ident = getPrefix code
              rest = stripPrefix code
              newPos =
                if hasNewLine rest
                  then (line + 1, countSpacesInFront rest)
                  else (line, column + T.length ident)
           in parseIdent (T.strip ident) line (column + T.length ident) :
              tokenize (T.strip rest) (fst newPos) (snd newPos)
        False ->
          case T.length code >= 2 &&
               isTwoPlaceOperator (T.pack ([c] <> [(T.head (T.tail code))])) of
            True ->
              parseDoubleOperator
                (T.pack $ [c] <> [(T.head (T.tail code))])
                line
                column :
              tokenize (T.tail (T.tail code)) line (column + 2)
            False ->
              case isOnePlaceOperator c of
                True ->
                  parseSingleOperator c line column :
                  tokenize (T.tail code) line (column + 1)
                False ->
                  case isWhitespace c of
                    True -> tokenize (T.tail code) line (column + 1)
                    False ->
                      case isNewLine c of
                        True -> tokenize (T.tail code) (line + 1) 0
                        False ->
                          case isAlphaNum c of
                            True ->
                              let num = getDecNumber code
                                  next = T.drop (T.length num) code
                               in tokenNumber num line column :
                                  tokenize next line (column + T.length num)
                            False ->
                              case c == '"' of
                                True ->
                                  let str = getStringLiteral (T.drop 1 code)
                                      next = T.drop (T.length str) code
                                   in tokenStringLiteral str line column :
                                      tokenize next line (column + T.length str)
                                False -> tokenize (T.tail code) line column
