{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Char8         as C8 (putStrLn, pack)
import           Language.Bash.PrettyPrinter
import           Language.PureShell.Procedural

printBash :: ToBashStatement s => s -> IO ()
printBash = C8.putStrLn . bytes . toBashStatement

example1 = do
  let boo = Language.PureShell.Procedural.Literal "boo"
  let s = FunDef @ByteString (FunName "hello") [] (Sequence [] boo)
  printBash s

example2 = do
  let hello = Language.PureShell.Procedural.Literal "hello"
  let f1 = FunDef @ByteString (FunName "concat") [VarName "left", VarName "right"] (Sequence as p)
        where
          as = [ Assignment (VarName "pattern") $ Sequence [] $ Literal "%s%s" ]
          p = Application (ClosureFromName $ FunName "printf") [VarName "pattern", VarName "left", VarName "right"]
  let f2 = FunDef @ByteString (FunName "hello") [VarName "name"] (Sequence as p)
        where
          as = [Assignment (VarName "hello") $ Sequence [] $ Literal "hello"]
          p = Application (ClosureFromName $ FunName "concat") [VarName "hello", VarName "name"]
  let m = Module [f1, f2]
  printBash m

example3 = do
  let project12 = FunDef @ByteString (FunName "project12") ps (Sequence [] p)
        where
          ps = fmap (VarName . C8.pack . ("local" <>) . show) [1..12]
          p = Application (ClosureFromName $ FunName "echo") [VarName "local12"]
  printBash project12

example4 = do
  let v = VarName "bla"
  let v1 = VarName "baz"  
  let objectFoo = FunDef @ByteString (FunName "objectFoo") [v] (Sequence as p)
        where
          as = fmap ObjectCommand [ EmptyObject o
                                  , UpdateField o f v
                                  , EncodeObject v1 o
                                  ]
          o = ObjectName "bar"
          f = FieldName "bar"
          p = Application (ClosureFromName $ FunName "echo") [v1]
  printBash objectFoo

example5 = do
  let v = VarName "bla"
  let w = VarName "brr"
  let v1 = VarName "baz"  
  let objectFoo = FunDef @ByteString (FunName "objectBar") [w, v] (Sequence as p)
        where
          as = fmap ObjectCommand [ DecodeObject o w 
                                  , UpdateField o f v
                                  , EncodeObject v1 o
                                  ]
          o = ObjectName "bar"
          f = FieldName "bar"
          p = Application (ClosureFromName $ FunName "echo") [v1] -- TODO this can actually be simplified
  printBash objectFoo

example6 = do
  let d = VarName "default"
  let f = VarName "f"
  let v = VarName "v"
  let o = ObjectName "vPrime"
  let r = VarName "r"
  let tag = VarName "tag"
  let tag' = FieldName "tag"
  let value = VarName "value"
  let value' = FieldName "value"
  let s = [ ObjectCommand $ DecodeObject o v
          , ObjectCommand $ ProjectField tag o tag' 
          , Case r tag [ CaseBranch "Nothing" $ Sequence [] $ Application (ClosureFromName $ FunName "echo") [d]
                       , CaseBranch "Just" $ Sequence [ObjectCommand $ ProjectField value o value' ] $
                         Application (ClosureFromVar f) [value]
                       ]
          ]
  let body = Sequence s $ Application (ClosureFromName $ FunName "echo") [r]
  let maybeFoo = FunDef @ByteString (FunName "maybe") [d, f, v] body
  printBash maybeFoo

main :: IO ()
main = do
  example1
  example2
  example3
  example4
  example5
  example6
