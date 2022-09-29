{-# LANGUAGE PolyKinds #-}

module Language.PureShell.Context.Literals where

import           Language.PureScript.PSString (PSString)

data Literal a e c
   = NumericLiteral (Either Integer Double)
   | StringLiteral PSString
   | CharLiteral Char
   | BooleanLiteral Bool
   | ArrayLiteral [e a c]
   | ObjectLiteral [(PSString, e a c)]
