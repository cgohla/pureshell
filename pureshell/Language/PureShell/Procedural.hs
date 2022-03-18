{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Language.PureShell.Procedural where

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as C8 (pack)
import qualified Data.ByteString.ShellEscape as Escape (bash)
import qualified Language.Bash               as Bash (Annotated (..),
                                                      Assignment (..),
                                                      Expression (..),
                                                      FuncName (..),
                                                      Statement (..),
                                                      VarName (..), literal)

import qualified Language.Bash.Syntax        as Bash (Identifier (..),
                                                      SpecialVar (..))
import qualified Language.Bash.Test          as Bash (Test (..), test)

newtype FunName = FunName { getFunName :: ByteString } deriving newtype (Show, Eq, Ord)

newtype VarName = VarName { getVarName :: ByteString } deriving newtype (Show, Eq, Ord)

data Module l = Module [FunDef l] deriving (Show, Eq, Ord)

data FunClosure = ClosureFromName FunName
                | ClosureFromVar VarName deriving (Show, Eq, Ord)

data Statement l = Literal l
                 | Application FunClosure [VarName]
  deriving (Show, Eq, Ord)

data Assignment l = Assignment VarName (Statement l) deriving (Show, Eq, Ord)

data FunDef l = FunDef FunName [VarName] [Assignment l] (Statement l) deriving (Show, Eq, Ord)

class ToBashExpression a where
  toBashExpression :: a -> Bash.Expression ()

instance ToBashExpression ByteString where
  toBashExpression l = Bash.Literal $ Escape.bash l

class ToBashStatement a where
  toBashStatement :: a -> Bash.Statement ()

instance (ToBashExpression l) => ToBashStatement (Statement l) where
  toBashStatement = \case
    Literal l -> Bash.SimpleCommand (Bash.literal "echo") [(toBashExpression l)]
    Application f vs-> Bash.SimpleCommand f' vs'
      where
        vs' = fmap (readFromVar . getVarName) vs
        f' = case f of
               ClosureFromName n -> Bash.Literal $ Escape.bash $ getFunName n
               ClosureFromVar v  -> readFromVar $ getVarName v
        readFromVar = Bash.ReadVarSafe . Bash.VarIdent . Bash.Identifier

appendBashStatements :: [Bash.Statement ()] -> Bash.Statement ()
appendBashStatements [] = Bash.Empty
appendBashStatements (s:ss) = Bash.Sequence (ann s) $ ann $ appendBashStatements ss
  where
    ann = Bash.Annotated ()

instance (ToBashExpression l) => ToBashStatement (FunDef l)  where
  toBashStatement (FunDef (FunName n) ns as s) = Bash.Function n' a'
    where
      n' = Bash.Fancy n
      entry = [Bash.IfThen c r]
        where
          c = Bash.Annotated () $ Bash.test $ Bash.ARGVLength `Bash.Test_lt` length' ns
          length' xs = Bash.Literal $ Escape.bash $ C8.pack $ show $ length xs
          r = Bash.Annotated () $ appendBashStatements [echoClosure, exit]
          echoClosure = Bash.SimpleCommand (Bash.Literal $ Escape.bash "echo")
                        [Bash.Literal $ Escape.bash n, Bash.ARGVElements]
          exit = Bash.SimpleCommand (Bash.Literal $ Escape.bash "exit")
                 [Bash.Literal $ Escape.bash "0"]
      ps = fmap posbind $ zip [1..] ns
        where
          posbind (i, v) = Bash.Assign $ Bash.Var (Bash.Identifier $ getVarName v) $ Bash.ReadVar $ Bash.VarSpecial (dollar i) -- TODO use shifting to assign the parameters
          -- TODO need to declare local variables
          dollar 0 = Bash.Dollar0
          dollar 1 = Bash.Dollar1
          dollar 2 = Bash.Dollar2
          dollar 3 = Bash.Dollar3
          dollar 4 = Bash.Dollar4
          dollar 5 = Bash.Dollar5
          dollar 6 = Bash.Dollar6
          dollar 7 = Bash.Dollar7
          dollar 8 = Bash.Dollar8
          dollar 9 = Bash.Dollar9
          dollar _ = error "unsupported positional parameter" -- TODO this is a problem
      as'= fmap assignment as
        where
          assignment (Assignment v s) = Bash.Assign $ Bash.Var i e
            where
              i = Bash.Identifier $ getVarName v
              e = Bash.Eval $ Bash.Annotated () $ toBashStatement s
      a' = Bash.Annotated () $ appendBashStatements $ entry <> ps <> as' <> [toBashStatement s]

instance (ToBashExpression l) => ToBashStatement (Module l) where
  toBashStatement (Module fs) = go fs
    where
      go [] = Bash.Empty
      go (f:fs) = Bash.Sequence a b
        where
          a = Bash.Annotated () $ toBashStatement f
          b = Bash.Annotated () $ go fs
