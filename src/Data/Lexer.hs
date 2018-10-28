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
import           Debug.Trace

parseIdent :: T.Text -> Line -> Column -> Token
parseIdent ident line column
  | MapS.member ident keywords = token keywords ident line column
  | MapS.member ident predeclIdentifiers =
    token predeclIdentifiers ident line column
  | otherwise =
    Token
      { tokenType = Ident (Identifier ident)
      , lexeme = ident
      , location = (line, column)
      }

parseSingleOperator :: Char -> Line -> Column -> Token
parseSingleOperator c line column = singleOp
  where
    singleOp = token operators (T.singleton c) line (column + 1)

parseDoubleOperator :: T.Text -> Line -> Column -> Token
parseDoubleOperator op line column = doubleOp
  where
    doubleOp = token operators op line (column + 2)

tokenize :: T.Text -> Line -> Column -> [Token]
-- tokenize code line column
--   | trace ("\nCode is: " <> (T.unpack code)) False = undefined
tokenize code line column =
  case safeHead code of
    Nothing -> []
    Just c ->
      case isAlpha c || c == '_' of
        True ->
          let ident = getPrefix code
              rest = stripPrefix code
           in parseIdent (T.strip ident) line (column + T.length ident) :
              tokenize rest line (column + T.length ident - 1)
        False ->
          let twoAhead = T.pack ([c] <> [(T.head (T.tail code))])
              skipTwo = T.tail $ T.tail code
           in case T.length code >= 2 && isTwoPlaceOperator twoAhead of
                True ->
                  parseDoubleOperator twoAhead line column :
                  tokenize skipTwo line (column + 2)
                False ->
                  case isOnePlaceOperator c of
                    True ->
                      parseSingleOperator c line column :
                      tokenize (T.tail code) line (column + 1)
                    False ->
                      case isWhitespace c of
                        True ->
                          tokenSpace " " line column :
                          tokenize (T.tail code) line (column + 1)
                        False ->
                          case isNewLine c of
                            True ->
                              tokenSpace "\n" line column :
                              tokenize (T.tail code) (line + 1) 0
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
                                          tokenize
                                            next
                                            line
                                            (column + T.length str)
                                    False -> tokenize (T.tail code) line column
