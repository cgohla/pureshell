{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Language.PureShell.ContextCoreFn.CodeGen where

import qualified Language.PureShell.ContextCoreFn.IR as X (Bind (NonRec, Rec),
                                                           BindList (..),
                                                           Binder (..),
                                                           BinderList (..),
                                                           LitBinder (..),
                                                           LitBinderArray (..),
                                                           LitBinderObject (..),
                                                           RecList (..))

import           Control.Applicative.Singletons      (SApplicative (sPure))
import           Data.List.Singletons                (SList (..), (%++))
import           Data.Singletons                     (Sing)


getRecListVars :: X.RecList a l c -> Sing l
getRecListVars = \case
  X.RecNil             -> SNil
  X.RecCons _a i _e rs -> SCons i $ getRecListVars rs

getBindVars :: X.Bind a l c -> Sing l
getBindVars = \case
  X.NonRec _a i _e -> sPure i
  X.Rec rs         -> getRecListVars rs

getBindListVars :: X.BindList a l c -> Sing l
getBindListVars = \case
  X.BindListNil       -> SNil
  X.BindListCons b bs ->  getBindVars b %++ getBindListVars bs

getArrayBinderVars :: X.LitBinderArray a l -> Sing l
getArrayBinderVars = \case
  X.LitBinderArrayNil       -> SNil
  X.LitBinderArrayCons b bs -> getBinderVars b %++ getArrayBinderVars bs

getObjectBinderVars :: X.LitBinderObject a l -> Sing l
getObjectBinderVars = \case
  X.LitBinderObjectNil          -> SNil
  X.LitBinderObjectCons _f b bs -> getBinderVars b %++ getObjectBinderVars bs

getLitBinderVars :: X.LitBinder a l -> Sing l
getLitBinderVars = \case
  X.NumericLitBinder _ -> SNil
  X.StringLitBinder _  -> SNil
  X.CharLitBinder _    -> SNil
  X.BooleanLitBinder _ -> SNil
  -- ^ It's a bit annoying we can't use a null binder for all of these
  X.ArrayLitBinder a   -> getArrayBinderVars a
  X.ObjectLitBinder o  -> getObjectBinderVars o

getBinderVars :: X.Binder a l -> Sing l
getBinderVars = \case
  X.NullBinder _a                   -> SNil
  X.LiteralBinder _a l              -> getLitBinderVars l
  X.VarBinder _a i                  -> sPure i
  X.ConstructorBinder _a _tn _cn bs -> getBinderListVars bs
  X.NamedBinder _a i b              -> SCons i $ getBinderVars b

getBinderListVars :: X.BinderList a l -> Sing l
getBinderListVars = \case
  X.BinderListNil       -> SNil
  X.BinderListCons b bs -> getBinderVars b %++ getBinderListVars bs

