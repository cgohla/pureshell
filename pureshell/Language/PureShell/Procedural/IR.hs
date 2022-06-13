{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module Language.PureShell.Procedural.IR where

import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8          as C8 (pack)
import qualified Data.ByteString.ShellEscape    as Escape (bash)
import qualified Language.Bash                  as Bash (Annotated (..),
                                                         Assignment (..),
                                                         Expression (..),
                                                         FuncName (..),
                                                         Statement (..),
                                                         VarName (..), literal)
import qualified Language.Bash.Syntax           as Bash (Identifier (..),
                                                         SpecialVar (..),
                                                         Trim (..))

import qualified Language.Bash.Test             as Bash (Test (..), test)
import qualified Language.PureShell.Identifiers as Ids

newtype ObjectName = ObjectName { getObjectName :: ByteString } deriving newtype (Show, Eq, Ord)

newtype FieldName = FieldName { getFieldName :: ByteString } deriving newtype (Show, Eq, Ord)

data Module lit = Module [FunDef lit] deriving (Show, Eq, Ord)

instance Semigroup (Module lit) where
  (Module m) <> (Module n) = Module (n <> m)

instance Monoid (Module lit) where
  mempty = Module []

data FunClosure = ClosureFromName Ids.SimpleBashFunName
                | ClosureFromVar Ids.LocalBashVarName deriving (Show, Eq, Ord)

data ObjectCommand = EmptyObject ObjectName
                   | DecodeObject ObjectName Ids.LocalBashVarName
                   | EncodeObject Ids.LocalBashVarName ObjectName
                   | UpdateField ObjectName FieldName Ids.LocalBashVarName
                   | ProjectField Ids.LocalBashVarName ObjectName FieldName
                 deriving (Show, Eq, Ord)

data Expression lit = Literal lit
                   | Application FunClosure [Ids.LocalBashVarName]
                   -- TODO we may want to add  another indirection layer here to allowliteral params
                   | Variable Ids.LocalBashVarName -- NOTE this seems clunky
                   deriving (Show, Eq, Ord)

data Sequence lit = Sequence [Assignment lit] (Expression lit) deriving (Show, Eq, Ord)

data CaseBranch lit = CaseBranch lit (Sequence lit) deriving (Show, Eq, Ord)

data Assignment lit = Assignment Ids.LocalBashVarName (Sequence lit)
                  | ObjectCommand ObjectCommand
                  | Case Ids.LocalBashVarName Ids.LocalBashVarName [CaseBranch lit]
                  deriving (Show, Eq, Ord)

data FunDef lit = FunDef Ids.SimpleBashFunName [Ids.LocalBashVarName] (Sequence lit) deriving (Show, Eq, Ord)
