{-# LANGUAGE OverloadedStrings #-}

module Data.Highlighter
  ( tokensToHtml
  ) where

import qualified Data.Text       as T
import qualified Data.Text.IO    as TI

import           Data.Maybe      (fromJust)

import           Data.Token      (Token (..))
import           Data.TokenTypes

initialHtml :: T.Text
initialHtml =
  T.unlines
    [ "<html>"
    , "<head>"
    , "<style>"
    , ".keyword {\n color: orange; \n}"
    , ".preident {\n color: red; \n}"
    , ".operator {\n color: black; \n}"
    , ".ident {\n color: blue; \n}"
    , ".string {\n color: green; \n}"
    , ".number {\n color: purple; \n}"
    , "</style>"
    , "</head>"
    , "<body>"
    ]

toHtml :: Token -> T.Text
toHtml (Token {tokenType = Keyword _, lexeme = lex, location = (_, _)}) =
  "<span class=keyword>" <> lex <> "</span>"
toHtml (Token {tokenType = PredeclIdent _, lexeme = lex, location = (_, _)}) =
  "<span class=preident>" <> lex <> "</span>"
toHtml (Token {tokenType = Operator _, lexeme = lex, location = (_, _)}) =
  "<span class=operator>" <> lex <> "</span>"
toHtml (Token {tokenType = Ident _, lexeme = lex, location = (_, _)}) =
  "<span class=ident>" <> lex <> "</span>"
toHtml (Token { tokenType = Literal (StringLiteral _)
              , lexeme = lex
              , location = (_, _)
              }) = "<span class=string>" <> lex <> "</span>"
toHtml (Token {tokenType = Literal (Number _), lexeme = lex, location = (_, _)}) =
  "<span class=number>" <> lex <> "</span>"

tokensToHtml :: [Maybe Token] -> T.Text
tokensToHtml tokens =
  initialHtml <> T.concat (map (toHtml . fromJust) tokens) <> "</body>\n</html>"
