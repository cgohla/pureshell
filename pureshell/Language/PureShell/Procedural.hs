{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
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
                                                      SpecialVar (..),
                                                      Trim (..))
import qualified Language.Bash.Test          as Bash (Test (..), test)

newtype FunName = FunName { getFunName :: ByteString } deriving newtype (Show, Eq, Ord)

newtype VarName = VarName { getVarName :: ByteString } deriving newtype (Show, Eq, Ord)

newtype ObjectName = ObjectName { getObjectName :: ByteString } deriving newtype (Show, Eq, Ord)

newtype FieldName = FieldName { getFieldName :: ByteString } deriving newtype (Show, Eq, Ord)

data Module l = Module [FunDef l] deriving (Show, Eq, Ord)

data FunClosure = ClosureFromName FunName
                | ClosureFromVar VarName deriving (Show, Eq, Ord)

data ObjectCommand = EmptyObject ObjectName
                   | DecodeObject ObjectName VarName
                   | EncodeObject VarName ObjectName
                   | UpdateField ObjectName FieldName VarName
                   | ProjectField VarName ObjectName FieldName
                 deriving (Show, Eq, Ord)

data Statement l = Literal l -- TODO we should probably rename this to 'Expression'
                 | Application FunClosure [VarName]
                 -- perhaps we also need variable reads here
                 deriving (Show, Eq, Ord)

data Sequence l = Sequence [Assignment l] (Statement l) deriving (Show, Eq, Ord)

data CaseBranch l = CaseBranch l (Sequence l) deriving (Show, Eq, Ord)

data Assignment l = Assignment VarName (Sequence l)
                  | ObjectCommand ObjectCommand
                  | Case VarName VarName [CaseBranch l]
                  deriving (Show, Eq, Ord)

data FunDef l = FunDef FunName [VarName] (Sequence l) deriving (Show, Eq, Ord)

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

instance ToBashStatement (ObjectCommand) where
  toBashStatement = \case
    EmptyObject o     -> Bash.Declare $ Bash.Dict (Bash.Identifier $ getObjectName o) []
    DecodeObject o v  -> Bash.SimpleCommand d [a, v']
      where
        -- We have to build this command using literals
        -- unfortunately. 'Assignment.Dict' is no flexible enough.
        d  = Bash.Literal $ Escape.bash "declare"
        a  = Bash.Literal $ Escape.bash "-A"
        v' = Bash.UnescapedLiteral $ (getObjectName o) <> "=\"${" <> getVarName v <> "}\""
    EncodeObject v o  -> appendBashStatements $ fmap (Bash.Assign . v') [e, (Bash.Trim Bash.ShortestLeading v'' p)]
      where
        v'  = Bash.Var w
        v'' = Bash.VarIdent w
        w   = Bash.Identifier $ getVarName v
        o'  = Bash.Literal $ Escape.bash $ getObjectName o
        e   = Bash.Eval $ Bash.Annotated () $ Bash.SimpleCommand d [Bash.Literal $ Escape.bash "-p", o']
        d   = Bash.Literal $ Escape.bash "declare"
        p   = Bash.Literal $ Escape.bash $ "*="
    UpdateField o f v -> Bash.Assign $ Bash.Var field update
      where
        field  = Bash.Identifier $ getObjectName o <> "[" <> getFieldName f <> "]" -- TODO this should use DictUpdate
        update = Bash.ReadVar $ Bash.VarIdent $ Bash.Identifier $ getVarName v
    ProjectField v o f -> Bash.Assign $ v' (Bash.ReadArray o' f')
      where
        v' = Bash.Var $ Bash.Identifier $ getVarName v
        o' = Bash.Identifier $ getObjectName o
        f' = Bash.Literal $ Escape.bash $ getFieldName f

instance (ToBashExpression l) => ToBashStatement (Assignment l) where
  toBashStatement = \case
    Assignment v (Sequence as s) -> appendBashStatements [ss, Bash.Local $ Bash.Var i e]
      where
        ss = toBashStatement as
        i = Bash.Identifier $ getVarName v
        e = Bash.Eval $ Bash.Annotated () $ toBashStatement s
    ObjectCommand o -> toBashStatement o
    Case v x bs -> Bash.Case x' bs'
      where
        x' = Bash.ReadVar $ Bash.VarIdent $ Bash.Identifier $ getVarName x
        bs' = fmap branch bs
          where
            branch (CaseBranch l s) = (toBashExpression l, p)
              where
                p = Bash.Annotated () $ toBashStatement $ Assignment v s

appendBashStatements :: [Bash.Statement ()] -> Bash.Statement ()
appendBashStatements [] = Bash.Empty
appendBashStatements (s:ss) = Bash.Sequence (ann s) $ ann $ appendBashStatements ss
  where
    ann = Bash.Annotated ()

instance (ToBashExpression l) => ToBashStatement [Assignment l] where
  toBashStatement = appendBashStatements . fmap toBashStatement

instance (ToBashExpression l) => ToBashStatement (FunDef l)  where
  toBashStatement (FunDef (FunName n) ns (Sequence as s)) = Bash.Function n' a'
    where
      n' = Bash.Fancy n
      a' = Bash.Annotated () $ appendBashStatements $ entry <> ps <> [as', toBashStatement s]
      entry = [Bash.IfThen c r]
        where
          c = Bash.Annotated () $ Bash.test $ Bash.ARGVLength `Bash.Test_lt` length' ns
            where
              length' xs = Bash.Literal $ Escape.bash $ C8.pack $ show $ length xs
          r = Bash.Annotated () $ appendBashStatements [echoClosure, exit]
            where
              echoClosure = Bash.SimpleCommand (Bash.Literal $ Escape.bash "echo")
                            [Bash.Literal $ Escape.bash n, Bash.ARGVElements]
              exit = Bash.SimpleCommand (Bash.Literal $ Escape.bash "exit")
                     [Bash.Literal $ Escape.bash "0"]
      ps = fmap posbind $ zip [1..] ns
        where
          posbind (i, v) = Bash.Local $ Bash.Var (bindvar v) $ Bash.ReadVar $ posvar i
          bindvar = Bash.Identifier . getVarName
          posvar = Bash.VarSpecial . Bash.DollarNat
      as'= toBashStatement as

instance (ToBashExpression l) => ToBashStatement (Module l) where
  toBashStatement (Module fs) = go fs
    where
      go [] = Bash.Empty
      go (g:gs) = Bash.Sequence a b
        where
          a = Bash.Annotated () $ toBashStatement g
          b = Bash.Annotated () $ go gs
