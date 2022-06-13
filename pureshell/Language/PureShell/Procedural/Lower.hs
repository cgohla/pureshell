{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.PureShell.Procedural.Lower where

import           Data.ByteString                       (ByteString)
import qualified Data.ByteString.Char8                 as C8 (pack)
import qualified Data.ByteString.ShellEscape           as Escape (bash)
import qualified Language.Bash                         as Bash (Annotated (..),
                                                                Assignment (..),
                                                                Expression (..),
                                                                FuncName (..),
                                                                Statement (..),
                                                                VarName (..),
                                                                literal)
import qualified Language.Bash.Syntax                  as Bash (Identifier (..),
                                                                SpecialVar (..),
                                                                Trim (..))

import qualified Language.Bash.Test                    as Bash (Test (..), test)
import qualified Language.PureShell.Identifiers        as Ids
import qualified Language.PureShell.Procedural.CodeGen as P
import qualified Language.PureShell.Procedural.IR      as P


class ToBashExpression a where
  toBashExpression :: a -> Bash.Expression ()

instance ToBashExpression ByteString where
  toBashExpression l = Bash.Literal $ Escape.bash l

instance (ToBashExpression l) => ToBashExpression (P.Expression l) where
  toBashExpression = \case
    P.Literal l -> toBashExpression l
    P.Application f vs-> Bash.Eval $ Bash.Annotated () $ P.applicationAsStatement f vs
    P.Variable i -> Bash.ReadVarSafe $ Bash.VarIdent $ Bash.Identifier $ Ids.getVarName i

class ToBashStatement a where
  toBashStatement :: a -> Bash.Statement ()

instance ToBashStatement (P.ObjectCommand) where
  toBashStatement = \case
    P.EmptyObject o     -> Bash.Declare $ Bash.Dict (Bash.Identifier $ P.getObjectName o) []
    P.DecodeObject o v  -> Bash.SimpleCommand d [a, v']
      where
        -- We have to build this command using literals
        -- unfortunately. 'Assignment.Dict' is no flexible enough.
        d  = Bash.literal "declare"
        a  = Bash.literal "-A"
        v' = Bash.UnescapedLiteral $ (P.getObjectName o) <> "=\"${" <> Ids.getVarName v <> "}\""
    P.EncodeObject v o  -> P.appendBashStatements $ fmap (Bash.Assign . v') [e, (Bash.Trim Bash.ShortestLeading v'' p)]
      where
        v'  = Bash.Var w
        v'' = Bash.VarIdent w
        w   = Bash.Identifier $ Ids.getVarName v
        o'  = Bash.literal $ P.getObjectName o
        e   = Bash.Eval $ Bash.Annotated () $ Bash.SimpleCommand d [Bash.literal "-p", o']
        d   = Bash.literal "declare"
        p   = Bash.literal "*="
    P.UpdateField o f v -> Bash.Assign $ Bash.Var field update
      where
        field  = Bash.Identifier $ P.getObjectName o <> "[" <> P.getFieldName f <> "]" -- TODO this should use DictUpdate
        update = Bash.ReadVar $ Bash.VarIdent $ Bash.Identifier $ Ids.getVarName v
    P.ProjectField v o f -> Bash.Assign $ v' (Bash.ReadArray o' f')
      where
        v' = Bash.Var $ Bash.Identifier $ Ids.getVarName v
        o' = Bash.Identifier $ P.getObjectName o
        f' = Bash.literal $ P.getFieldName f

instance (ToBashExpression l) => ToBashStatement (P.Assignment l) where
  toBashStatement = \case
    P.Assignment v (P.Sequence as s) -> P.appendBashStatements [ss, Bash.Local $ Bash.Var i e]
      where
        ss = toBashStatement as
        i = Bash.Identifier $ Ids.getVarName v
        e = toBashExpression s
    P.ObjectCommand o -> toBashStatement o
    P.Case v x bs -> Bash.Case x' bs'
      where
        x' = Bash.ReadVar $ Bash.VarIdent $ Bash.Identifier $ Ids.getVarName x
        bs' = fmap branch bs
          where
            branch (P.CaseBranch l s) = (toBashExpression l, p)
              where
                p = Bash.Annotated () $ toBashStatement $ P.Assignment v s

instance (ToBashExpression l) => ToBashStatement [P.Assignment l] where
  toBashStatement = P.appendBashStatements . fmap toBashStatement

instance (ToBashExpression l) => ToBashStatement (P.FunDef l)  where
  toBashStatement (P.FunDef (Ids.SimpleBashFunName n) ns (P.Sequence as s)) = Bash.Function n' a'
    where
      n' = Bash.Fancy n
      a' = Bash.Annotated () $ P.appendBashStatements $ entry <> ps <> [as', expressionAsStatement s]
      expressionAsStatement e = case e of
        P.Application f vs -> P.applicationAsStatement f vs
        _ -> Bash.SimpleCommand (Bash.literal "echo") [toBashExpression e]
      entry = case ns of
        [] -> mempty
        _ -> [Bash.IfThen c r]
          where
            c = Bash.Annotated () $ Bash.test $ Bash.ARGVLength `Bash.Test_lt` length' ns
              where
                length' xs = Bash.literal $ C8.pack $ show $ length xs
            r = Bash.Annotated () $ P.appendBashStatements [echoClosure, exit]
              where
                echoClosure = Bash.SimpleCommand (Bash.literal "echo") [Bash.literal n, Bash.ARGVElements]
                exit = Bash.SimpleCommand (Bash.literal "return") [Bash.literal "0"]
      ps = fmap posbind $ zip [1..] ns
        where
          posbind (i, v) = Bash.Local $ Bash.Var (bindvar v) $ Bash.ReadVar $ posvar i
          bindvar = Bash.Identifier . Ids.getVarName
          posvar = Bash.VarSpecial . Bash.DollarNat
      as'= toBashStatement as

instance (ToBashExpression l) => ToBashStatement (P.Module l) where
  toBashStatement (P.Module fs) = go fs
    where
      go [] = Bash.Empty
      go (g:gs) = Bash.Sequence a b
        where
          a = Bash.Annotated () $ toBashStatement g
          b = Bash.Annotated () $ go gs
