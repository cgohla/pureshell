{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Char8               as C8 (pack, putStrLn)
import           Language.Bash.PrettyPrinter
import           Language.PureShell.Identifiers
import           Language.PureShell.Procedural.IR    as P
import           Language.PureShell.Procedural.Lower

printBash :: ToBashStatement s => s -> IO ()
printBash = C8.putStrLn . bytes . toBashStatement

example1 = do
  let boo = P.Literal "boo"
  let s = FunDef @ByteString (SimpleBashFunName "hello") [] (Sequence [] boo)
  printBash s

example2 = do
  let hello = P.Literal "hello"
  let f1 = FunDef @ByteString (SimpleBashFunName "concat") [LocalBashVarName "left", LocalBashVarName "right"] (Sequence as p)
        where
          as = [ Assignment (LocalBashVarName "pattern") $ Sequence [] $ Literal "%s%s" ]
          p = Application (ClosureFromName $ SimpleBashFunName "printf") [LocalBashVarName "pattern", LocalBashVarName "left", LocalBashVarName "right"]
  let f2 = FunDef @ByteString (SimpleBashFunName "hello") [LocalBashVarName "name"] (Sequence as p)
        where
          as = [Assignment (LocalBashVarName "hello") $ Sequence [] $ Literal "hello"]
          p = Application (ClosureFromName $ SimpleBashFunName "concat") [LocalBashVarName "hello", LocalBashVarName "name"]
  let m = Module [f1, f2]
  printBash m

example3 = do
  let project12 = FunDef @ByteString (SimpleBashFunName "project12") ps (Sequence [] p)
        where
          ps = fmap (LocalBashVarName . C8.pack . ("local" <>) . show) [1..12]
          p = Application (ClosureFromName $ SimpleBashFunName "echo") [LocalBashVarName "local12"]
  printBash project12

example4 = do
  let v = LocalBashVarName "bla"
  let v1 = LocalBashVarName "baz"
  let objectFoo = FunDef @ByteString (SimpleBashFunName "objectFoo") [v] (Sequence as p)
        where
          as = fmap ObjectCommand [ EmptyObject o
                                  , UpdateField o f v
                                  , EncodeObject v1 o
                                  ]
          o = ObjectName "bar"
          f = FieldName "bar"
          p = Application (ClosureFromName $ SimpleBashFunName "echo") [v1]
  printBash objectFoo

example5 = do
  let v = LocalBashVarName "bla"
  let w = LocalBashVarName "brr"
  let v1 = LocalBashVarName "baz"
  let objectFoo = FunDef @ByteString (SimpleBashFunName "objectBar") [w, v] (Sequence as p)
        where
          as = fmap ObjectCommand [ DecodeObject o w
                                  , UpdateField o f v
                                  , EncodeObject v1 o
                                  ]
          o = ObjectName "bar"
          f = FieldName "bar"
          p = Application (ClosureFromName $ SimpleBashFunName "echo") [v1] -- TODO this can actually be simplified
  printBash objectFoo

example6 = do
  let d = LocalBashVarName "default"
  let f = LocalBashVarName "f"
  let v = LocalBashVarName "v"
  let o = ObjectName "vPrime"
  let r = LocalBashVarName "r"
  let tag = LocalBashVarName "tag"
  let tag' = FieldName "tag"
  let value = LocalBashVarName "value"
  let value' = FieldName "value"
  let s = [ ObjectCommand $ DecodeObject o v
          , ObjectCommand $ ProjectField tag o tag'
          , Case r tag [ CaseBranch "Nothing" $ Sequence [] $ Application (ClosureFromName $ SimpleBashFunName "echo") [d]
                       , CaseBranch "Just" $ Sequence [ObjectCommand $ ProjectField value o value' ] $
                         Application (ClosureFromVar f) [value]
                       ]
          ]
  let body = Sequence s $ Application (ClosureFromName $ SimpleBashFunName "echo") [r]
  let maybeFoo = FunDef @ByteString (SimpleBashFunName "maybe") [d, f, v] body
  printBash maybeFoo

main :: IO ()
main = do
  example1
  example2
  example3
  example4
  example5
  example6
