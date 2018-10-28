{-# LANGUAGE OverloadedStrings #-}

module Data.Highlighter
  ( tokensToHtml
  ) where

import qualified Data.Text       as T
import qualified Data.Text.IO    as TI

import           Data.Maybe      (fromJust)

import           Data.Token
import           Data.TokenTypes

initialHtml :: T.Text
initialHtml =
  T.unlines
    [ "<html>"
    , "<head>"
    , "<style>"
    , ".keyword { color: orange; }"
    , ".preident { color: red; }"
    , ".operator { color: black; }"
    , ".ident { color: blue; }"
    , ".string { color: green; }"
    , ".number { color: purple; }"
    , "</style>"
    , "</head>"
    , "<body>"
    ]

toHtml :: Token -> T.Text
toHtml (Token {tokenType = Space _, lexeme = lex, location = (line, col)}) = lex
toHtml (Token {tokenType = Keyword _, lexeme = lex, location = (line, col)}) =
  "<span class=keyword>" <> lex <> "</span>"
toHtml (Token {tokenType = PredeclIdent _, lexeme = lex, location = (line, col)}) =
  "<span class=preident>" <> lex <> "</span>"
toHtml (Token {tokenType = Operator _, lexeme = lex, location = (line, col)}) =
  "<span class=operator>" <> lex <> "</span>"
toHtml (Token {tokenType = Ident _, lexeme = lex, location = (line, col)}) =
  "<span class=ident>" <> lex <> "</span>"
toHtml (Token { tokenType = Literal (StringLiteral _)
              , lexeme = lex
              , location = (line, col)
              }) = "<span class=string>" <> lex <> "</span>"
toHtml (Token { tokenType = Literal (Number _)
              , lexeme = lex
              , location = (line, col)
              }) = "<span class=number>" <> lex <> "</span>"

tokensToHtml :: [Maybe Token] -> T.Text
tokensToHtml tokens =
  initialHtml <> T.concat (map (toHtml . fromJust) tokens) <>
  "</body><br></html>"
