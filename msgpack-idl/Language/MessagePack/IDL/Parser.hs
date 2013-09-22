{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Language.MessagePack.IDL.Parser (
  idl,
  ) where

import Data.Char
import Data.Maybe
import qualified Data.Text as T
import Numeric
import Text.Peggy
import Text.Peggy.CodeGen.TH

import Language.MessagePack.IDL.Syntax

concatSurrogatePair :: [Char] -> [Char]
concatSurrogatePair [] = []
concatSurrogatePair (c : []) = [c]
concatSurrogatePair (h : l : [])
    | isSurrogatePair h l = [chr $ 0x10000 + ((ord h) - 0xD800) * 0x400 + ((ord l) - 0xDC00)]
    | otherwise           = [h,l]
concatSurrogatePair (h : l : rest) = (concatSurrogatePair [h,l]) ++ (concatSurrogatePair rest)

isSurrogatePair :: Char -> Char -> Bool
isSurrogatePair h l
    | ucp < 0xD800 = False
    | ucp > 0xDBFF = False
    | lcp < 0xDC00 = False
    | lcp > 0xDFFF = False
    | otherwise    = True
    where ucp = ord h
          lcp = ord l

genDecs $(peggyFile "mpidl.peggy")
