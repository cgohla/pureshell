{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Language.PureShell.CoreFn.LowerToContextCoreFn (lowerModule) where

import           Control.Applicative.Singletons      (sPure)
import           Data.Bifunctor                      (bimap, second)
import           Data.List                           (nub)
import           Data.List.Singletons                (SList (..), type (++),
                                                      (%++))
import           Data.Map.Strict                     as M (fromList, keys,
                                                           lookup, type Map)
import           Data.Maybe                          (fromMaybe)
import           Data.Singletons                     (Sing, fromSing,
                                                      withSomeSing)
import           Data.Singletons.Decide              (type (:~:) (..))
import           Data.List.Props                     (decideIsElem,
                                                      listRightUnit)

import qualified Language.PureShell.ContextCoreFn.IR as X
import qualified Language.PureShell.CoreFn.IR        as F

lowerModule :: F.Module a -> X.Module a
lowerModule m =
  withSomeSing (elaborateIdents $ F.moduleForeign m) $ \f ->
  withSomeSing (nub $ findImportedSymbols $ F.moduleDecls m) $ \i ->
  case (elaborateBindList (X.sImports i %++ X.sLocals f) $ F.moduleDecls m) of
    (SomeBindList _ d) ->
      X.Module
      { X.moduleSourceSpan = F.moduleSourceSpan m
      , X.moduleComments   = F.moduleComments m
      , X.moduleName       = case F.moduleName m of
                                F.ModuleName n -> X.ModuleName n
      , X.modulePath       = F.modulePath m
      , X.moduleImports    = i
      , X.moduleForeign    = f
      , X.moduleDecls      = d
      }

findImportedSymbols :: [F.Bind a] -> [X.Imported X.Ident]
findImportedSymbols = foldl findImportedSymbolsBind []
  where
    findImportedSymbolsBind acc = \case
      F.NonRec _ _ e -> findImportedSymbolsInExpr acc e
      F.Rec bs       -> foldl findImportedSymbolsInExpr acc $ snd <$> bs
    findImportedSymbolsInExpr acc = \case
      (F.Literal _ l)         -> findImportedSymbolsInLiteral acc l
      (F.Constructor _ _ _ _) -> acc
      (F.Accessor _ _ e)      -> findImportedSymbolsInExpr acc e
      (F.ObjectUpdate _ e us) -> let a = findImportedSymbolsInExpr acc e in
                                    foldl findImportedSymbolsInExpr a $ snd <$> us
      (F.Abs _ _ e)           -> findImportedSymbolsInExpr acc e
      (F.App _ e e')          -> let a = findImportedSymbolsInExpr acc e in
                                     findImportedSymbolsInExpr a e'
      (F.Var _ (F.Qualified (F.ByModuleName (F.ModuleName m)) (F.Ident i))) ->
        X.Imported (X.ModuleName m) (X.Ident i) : acc
        -- ^ NOTE This is the heart of it: An imported symbol is one
        -- qualified by a module name; all others by implication are local
        -- references. This assumption may turn out to be wrong.
      (F.Var _ _)             -> acc
      (F.Case _ es as)        -> let a = foldl findImportedSymbolsInExpr acc es in
                                      foldl findImportedSymbolsInCA a as
      (F.Let _ bs e)          -> let a = foldl findImportedSymbolsBind acc bs in
                                    findImportedSymbolsInExpr a e
    findImportedSymbolsInLiteral acc = \case
      (F.ArrayLiteral ls)  -> foldl findImportedSymbolsInExpr acc ls
      (F.ObjectLiteral rs) -> foldl findImportedSymbolsInExpr acc $ snd <$> rs
      _                    -> acc
    findImportedSymbolsInCA acc (F.CaseAlternative _ r) = case r of
      Left gdes -> let unpackGuards =
                         foldl (\a -> \(x,y) -> x:y:a) []
                   in
                     foldl findImportedSymbolsInExpr acc $
                     unpackGuards gdes
      Right e -> findImportedSymbolsInExpr acc e

elaborateIdents :: [F.Ident] -> [X.Ident]
elaborateIdents = fmap elaborateIdent

elaborateIdent :: F.Ident -> X.Ident
elaborateIdent (F.Ident t)      = X.Ident t
elaborateIdent (F.GenIdent t i) = X.GenIdent t (fromInteger i)
-- fromInteger is obviously partial and may crash
elaborateIdent F.UnusedIdent    = X.UnusedIdent
elaborateIdent _                = error "impossible"

elaborateQualifiedIdent :: F.Qualified F.Ident -> X.Qualified X.Ident
elaborateQualifiedIdent (F.Qualified m i) = X.Qualified (f m) $ elaborateIdent i
  where
    f (F.ByModuleName (F.ModuleName n)) = Just $ X.ModuleName n
    f (F.BySourcePos _s)                = Nothing
    -- ^ NOTE we can't actually use the source pos qualification,
    -- because the qualifieres are not given at CoreFn binding sites
    -- (Abs, Let, var binder).

elaborateLiteral :: Sing c -> F.Literal (F.Expr a) -> X.Literal a c
elaborateLiteral _ (F.NumericLiteral n) = X.NumericLiteral n
elaborateLiteral _ (F.StringLiteral s)  = X.StringLiteral s
elaborateLiteral _ (F.CharLiteral c)    = X.CharLiteral c
elaborateLiteral _ (F.BooleanLiteral b) = X.BooleanLiteral b
elaborateLiteral c (F.ArrayLiteral as)  = X.ArrayLiteral $
  fmap (elaborateExpr c) as
elaborateLiteral c (F.ObjectLiteral as)  = X.ObjectLiteral $
  fmap (second $ elaborateExpr c) as

data SomeLitBinderArray a = forall l. SomeLitBinderArray (Sing l) (X.LitBinderArray a l)

elaborateLitBinderArray :: [F.Binder a] -> SomeLitBinderArray a
elaborateLitBinderArray []       = SomeLitBinderArray SNil X.LitBinderArrayNil
elaborateLitBinderArray (b : bs) = case elaborateBinder b of
                                     SomeBinder l b' -> case elaborateLitBinderArray bs of
                                       SomeLitBinderArray l' bs' ->
                                         SomeLitBinderArray (l %++ l') $ X.LitBinderArrayCons b' bs'

data SomeLitBinder a = forall l. SomeLitBinder (Sing l) (X.LitBinder a l)

data SomeLitBinderObject a = forall l. SomeLitBinderObject (Sing l) (X.LitBinderObject a l)

elaborateLitBinderObject :: [(F.PSString, F.Binder a)] -> SomeLitBinderObject a
elaborateLitBinderObject []       = SomeLitBinderObject SNil X.LitBinderObjectNil
elaborateLitBinderObject ((f,b) : bs) = case elaborateBinder b of
                                          SomeBinder l b' -> case elaborateLitBinderObject bs of
                                            SomeLitBinderObject l' bs' ->
                                              SomeLitBinderObject (l %++ l') $ X.LitBinderObjectCons f b' bs'

elaborateLitBinder :: F.Literal (F.Binder a) -> SomeLitBinder a
elaborateLitBinder = \case
  F.NumericLiteral n      -> SomeLitBinder SNil $ X.NumericLitBinder n
  F.StringLiteral s       -> SomeLitBinder SNil $ X.StringLitBinder s
  F.CharLiteral c         -> SomeLitBinder SNil $ X.CharLitBinder c
  F.BooleanLiteral b      -> SomeLitBinder SNil $ X.BooleanLitBinder b
  F.ArrayLiteral a        -> case elaborateLitBinderArray a of
                               SomeLitBinderArray l a' -> SomeLitBinder l $ X.ArrayLitBinder a'
  F.ObjectLiteral o      -> case elaborateLitBinderObject o of
                               SomeLitBinderObject l o' -> SomeLitBinder l $ X.ObjectLitBinder o'

data SomeBinder a = forall l. SomeBinder (Sing l) (X.Binder a l)

elaborateBinder :: F.Binder a -> SomeBinder a
elaborateBinder b = case b of
  F.NullBinder a               -> SomeBinder SNil $ X.NullBinder a
  F.LiteralBinder a l          -> case elaborateLitBinder l of
                                    SomeLitBinder l' b' -> SomeBinder l' $ X.LiteralBinder a b'
  F.VarBinder a i              -> withSomeSing (elaborateIdent i) $
                                  \i' -> SomeBinder (sPure i') $ X.VarBinder a i'
  F.ConstructorBinder a t n bs -> case elaborateBinderList bs of
                                    SomeBinderList l bs' -> SomeBinder l $ X.ConstructorBinder a t n bs'
  F.NamedBinder a i z          -> withSomeSing (elaborateIdent i) $
                                  \i' -> case (elaborateBinder z) of
                                    SomeBinder l d -> SomeBinder (SCons i' l) $ X.NamedBinder a i' d

data SomeBinderList a = forall l. SomeBinderList (Sing l) (X.BinderList a l)

elaborateBinderList :: [F.Binder a] -> SomeBinderList a
elaborateBinderList []       = SomeBinderList SNil X.BinderListNil
elaborateBinderList (b : bs) = case elaborateBinder b of
                                 SomeBinder l b' -> case elaborateBinderList bs of
                                   SomeBinderList l' bs' -> SomeBinderList (l %++ l') $
                                     X.BinderListCons b' bs'

elaborateCaseAlternative :: Sing c -> F.CaseAlternative a -> X.CaseAlternative a c
elaborateCaseAlternative c (F.CaseAlternative bs rd) = case (elaborateBinderList bs) of
                                                         SomeBinderList l bs' -> case rd of
                                                           Left gs -> X.CaseAlternative bs' $
                                                                      Left $ fmap (bimap elab elab) gs
                                                           Right r -> X.CaseAlternative bs' $
                                                                      Right $ elab r
                                                           where
                                                             elab = elaborateExpr (X.sLocals l %++ c)

elaborateExpr :: Sing c -> F.Expr a -> X.Expr a c
elaborateExpr c (F.Literal a l)          = X.Literal a $ elaborateLiteral c l
elaborateExpr _ (F.Constructor a t n fs) = withSomeSing (elaborateIdents fs) $
                                           \fs' -> X.Constructor a t n fs'
elaborateExpr c (F.Accessor a f e)       = X.Accessor a f $ elaborateExpr c e
elaborateExpr c (F.ObjectUpdate a e fs)  = X.ObjectUpdate a (elaborateExpr c e) $
                                           fmap (second $ elaborateExpr c) fs
elaborateExpr c (F.Abs a i e)            = withSomeSing (elaborateIdent i) $
                                           \i' -> X.Abs a i' $ elaborateExpr (SCons (X.sLocal i') c) e
elaborateExpr c (F.App a e e')           = X.App a (elaborateExpr c e) (elaborateExpr c e')
elaborateExpr c (F.Var a i)              = withSomeSing (elaborateQualifiedIdent i) $
                                           \i' -> maybe err (X.Var a i') (decideIsElement i' c)
                                           where
                                             err = (error $ mconcat [ "Scope error in CoreFn"
                                                                    , ". var ref"
                                                                    , show i
                                                                    , ", actual context"
                                                                    , show $ fromSing c
                                                                    ]
                                                   )
elaborateExpr c (F.Case a es as)         = X.Case a (fmap (elaborateExpr c) es) $
                                           fmap (elaborateCaseAlternative c) as
elaborateExpr c (F.Let a bs e)           = case elaborateBindList c bs of
                                             SomeBindList l bs' ->
                                               X.Let a bs' $ elaborateExpr (X.sLocals l %++ c) e

elaborateRecList :: forall l c a. Sing l -> Sing c
                 -> Map X.Ident (a, F.Expr a)
                 -> X.RecList a l (X.Locals l ++ c)
elaborateRecList l c bs = case listRightUnit l of
                             Refl -> go l (X.sLocals l %++ c) X.RecNil
                             -- The compiler on its own can not figure
                             -- out that l ++ '[] ~ l for a variable
                             -- l, so we must supply a
                             -- proof; otherwise the return type of
                             -- this application of 'go' does not
                             -- match with the return type of our
                             -- enclosing function.
  where
    go :: Sing l'' -> Sing c'
       -> X.RecList a l' c' -> X.RecList a (l'' ++ l') c'
    go SNil _ rl = rl
    go (SCons l' ls') c' rl =
      let
        err = error "impossible"
        (a, e) = fromMaybe err $ M.lookup (fromSing l') bs
        e' = elaborateExpr c' e
        rl' = go ls' c' rl
      in X.RecCons a l' e' rl'

data SomeBindList a c = forall l. SomeBindList (Sing l) (X.BindList a l c)

data SomeBind a c = forall l. SomeBind (Sing l) (X.Bind a l c)

elaborateBind :: Sing c -> F.Bind a -> SomeBind a c
elaborateBind c (F.NonRec a i e) = withSomeSing (elaborateIdent i) $
                                   \i' -> SomeBind (sPure i') $ X.NonRec a i' $
                                   elaborateExpr c e
elaborateBind c (F.Rec bs) = let bs' = exprDict bs
                                 exprDict = M.fromList . fmap f
                                 f = \((a, i), e) -> (elaborateIdent i, (a, e))
                             in
                               withSomeSing (keys bs') $
                               \d -> SomeBind d $ X.Rec $ elaborateRecList d c bs'

elaborateBindList :: Sing c -> [F.Bind a] -> SomeBindList a c
elaborateBindList _ [] = SomeBindList SNil X.BindListNil
elaborateBindList c (b : bs) = case elaborateBind c b of
                            SomeBind l b' ->
                              case elaborateBindList c bs of
                                SomeBindList l' bs' ->
                                  SomeBindList (l %++ l') $ X.BindListCons b' bs'
