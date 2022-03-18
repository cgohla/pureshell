{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.PureShell where

import           Data.Binary.Builder               (toLazyByteString)
import qualified Data.ByteString                   as BS (ByteString, pack,
                                                          unpack)
import           Data.ByteString.Internal          (c2w)
import qualified Data.ByteString.Lazy              as B (ByteString, unpack)
import           Data.ByteString.ShellEscape       (bash, bytes)
import           Data.Text.Encoding                (encodeUtf8)

import qualified Language.PureScript.AST.Literals  as L (Literal (..))
import           Language.PureScript.CoreFn.Ann    (Ann)
import           Language.PureScript.CoreFn.Expr   (Bind (..), Expr (..))
import           Language.PureScript.CoreFn.Module (Module (..))
import           Language.PureScript.Names         (Ident (..), runIdent)
import           Language.PureScript.PSString      (PSString, decodeString)

import qualified Language.Bash.PrettyPrinter       as Bash (bytes)
import           Language.Bash.Script              (script)
import qualified Language.Bash.Syntax              as Bash (Annotated (..),
                                                            Assignment (..),
                                                            Expression (..),
                                                            FuncName (..),
                                                            Identifier (..),
                                                            Statement (..),
                                                            VarName (..))

import           Data.Maybe                        (fromMaybe)

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

toFuncName :: BS.ByteString -> Bash.FuncName
toFuncName b = Bash.Fancy $ BS.pack $ foldMap subst $ BS.unpack b
  where
    subst i = if or [i <= 47 , and [58 <= i, i <= 64], and [ 91 <= i, i <= 96], 123 <= i]
              then
                fmap c2w $ "p" <> (show $ fromEnum i)
              else
                pure i

identToName = toName . encodeUtf8 . runIdent

identToFuncName = toFuncName . encodeUtf8 . runIdent

convertExpr :: Expr a -> Bash.Statement ()
convertExpr = \case
  Abs _ x e -> mconcat [ defineFunction , returnName ]-- TODO 1. create unique name 2. create function using eval 3. echo the name
    where
      defineFunction = undefined -- Bash.EvalCommand [fText]
      fText = Bash.Literal $ bash $ Bash.bytes $ (Bash.Function fName fBody :: Bash.Statement ()) -- TODO
                                                                                                  -- Problem:
                                                                                                  -- the
                                                                                                  -- function
                                                                                                  -- name
                                                                                                  -- needs
                                                                                                  -- to
                                                                                                  -- come
                                                                                                  -- from
                                                                                                  -- a
                                                                                                  -- variable,
                                                                                                  -- that
                                                                                                  -- gets
                                                                                                  -- substituted
                                                                                                  -- before
                                                                                                  -- the
                                                                                                  -- eval
      fName = identToFuncName (Ident "x") -- TODO needs a unique name
      fBody = Bash.Annotated () $ convertExpr e
      returnName = Bash.SimpleCommand (Bash.Literal $ bash "echo") [Bash.ReadVar $ Bash.VarIdent $ Bash.Identifier "x"]
  Case _ es as -> Bash.Empty
  _ -> Bash.Empty

topLevelBinding :: Bind Ann -> Bash.Statement ()
topLevelBinding = \case
  NonRec _ i (Literal _ (L.StringLiteral s)) -> Bash.Assign $ Bash.Var j e
    where
      j = identToName i
      e = Bash.Literal $ bash $ encodeUtf8 $ fromMaybe "OOPS" $ decodeString s
  NonRec _ i (Constructor _ _t _c _is) -> Bash.Empty
  NonRec _ i (Accessor _ _f _e) -> Bash.Empty
  NonRec _ i (ObjectUpdate _ _e _o) -> Bash.Empty
  NonRec _ i (Abs _ x e) -> mconcat [ assignName
                                    , defineFunction
                                    ]
    where
      assignName = Bash.Assign $ Bash.Var j n
        where
          j = identToName i
          n = Bash.Literal $ bash $ encodeUtf8 $ runIdent i
      defineFunction = Bash.Function fName fBody
        where
          fName = identToFuncName i
          fBody = Bash.Annotated () $ convertExpr e
  NonRec _ i (App _ _e1 _e2) -> Bash.Empty
  NonRec _ i (Var _ _i) -> Bash.Empty
  NonRec _ i (Case _ _es _as) -> Bash.Empty
  NonRec _ i (Let _ _bs _es) -> Bash.Empty
  _ -> Bash.Empty
