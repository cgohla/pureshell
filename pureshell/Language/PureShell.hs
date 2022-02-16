{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Language.PureShell where

import qualified Data.ByteString.Lazy as B (ByteString, unpack)
import qualified Data.ByteString as BS (ByteString, pack, unpack)
import Data.Text.Encoding (encodeUtf8)

import qualified Language.PureScript.AST.Literals as L (Literal(..))
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (Bind(..), Expr(..))
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.Names (Ident, runIdent)
import Language.PureScript.PSString (PSString, decodeString)

import qualified Language.Bash.Syntax as Bash (Statement(..), Annotated(..)
                                              , Assignment(..), Identifier(..), Expression(..))
import Language.Bash.Script (script)

import Data.ByteString.ShellEscape (bash, bytes)
import Data.ByteString.Internal (c2w)

import Data.Binary.Builder (toLazyByteString)

import Data.Maybe (fromMaybe)

-- Orphan instance for Statement
instance Semigroup (Bash.Statement ()) where
  s <> t = Bash.Sequence (a s) (a t)
    where
      a = Bash.Annotated ()

instance Monoid (Bash.Statement ()) where
  mempty = Bash.Empty

-- | Take a module and return a ByteString
convert :: Module Ann -> B.ByteString
convert Module {..} = toLazyByteString $ script $ topLevelBindings moduleDecls

topLevelBindings :: [Bind Ann] -> Bash.Statement ()
topLevelBindings = foldMap topLevelBinding

toName :: BS.ByteString -> Bash.Identifier
toName b = Bash.Identifier $ BS.pack $ foldMap subst $ BS.unpack b
  where
    subst i = if or [i <= 47 , and [58 <= i, i <= 64], and [ 91 <= i, i <= 96], 123 <= i]
              then
                fmap c2w $ "p" <> (show $ fromEnum i)
              else
                pure i      

identToName = toName . encodeUtf8 . runIdent

topLevelBinding :: Bind Ann -> Bash.Statement ()
topLevelBinding = \case
  -- TODO needs other literals
  NonRec _ i (Literal _ (L.StringLiteral s)) -> Bash.Assign $ Bash.Var j e
    where
      j = identToName i
      e = Bash.Literal $ bash $ encodeUtf8 $ fromMaybe "OOPS" $ decodeString s
  NonRec _ i (Constructor _ _t _c _is) -> Bash.Empty
  NonRec _ i (Accessor _ _f _e) -> Bash.Empty
  NonRec _ i (ObjectUpdate _ _e _o) -> Bash.Empty
  NonRec _ i (Abs _ _i _e) -> Bash.Empty
  NonRec _ i (App _ _e1 _e2) -> Bash.Empty
  NonRec _ i (Var _ _i) -> Bash.Empty
  NonRec _ i (Case _ _es _as) -> Bash.Empty
  NonRec _ i (Let _ _bs _es) -> Bash.Empty
  _ -> Bash.Empty
