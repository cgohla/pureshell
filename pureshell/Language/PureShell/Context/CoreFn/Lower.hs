{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Language.PureShell.Context.CoreFn.Lower where

-- | Lower Context.CoreFn to Context.Split

import qualified Language.PureShell.Context.CoreFn.IR as X
import           Language.PureShell.Context.Ident
import           Language.PureShell.Context.Literals  (Literal (..))
import qualified Language.PureShell.Context.Split.IR  as S

import           Control.Applicative.Singletons
import           Data.Functor.Singletons
import           Data.List                            (delete, (\\))
import           Data.List.NonEmpty                   (NonEmpty (..))
import           Data.List.Props
import           Data.List.Singletons
import           Data.Singletons
import           Data.Singletons.Decide
import           Data.Tuple.Singletons


lowerModule :: X.Module a -> S.Module a
lowerModule X.Module{..} = let c = sImports moduleImports %++ sLocals moduleForeign
                           in
                             case funVarsSection c of
                               -- REMEMBER We have to actually match the
                               -- Refl; using a wildcard is not enough.
                               Refl -> case lowerBindList (S.sFunVars c) moduleDecls of
                                 SomeBindList d -> S.Module { S.moduleDecls = d
                                                            , ..
                                                            }


ap :: (a :~: b) -> f a :~: f b
ap Refl = Refl

varVarsSection :: Sing (a :: [k]) -> Keys (S.VarVars a) :~: a
varVarsSection SNil         = Refl
varVarsSection (SCons _ as) = ap $ varVarsSection as

varVarSection :: Sing a -> Fst (S.VarVar a) :~: a
varVarSection _ = Refl

funVarsSection :: Sing (a :: [k]) -> Keys (S.FunVars a) :~: a
funVarsSection SNil         = Refl
funVarsSection (SCons _ as) = ap $ funVarsSection as

funVarSection :: Sing a -> Fst (S.FunVar a) :~: a
funVarSection _ = Refl

consEq :: Sing a -> Sing as -> a :~: b -> as :~: bs -> (a ': as) :~: (b ': bs)
consEq _ _ Refl Refl = Refl

headEq :: (a ': as) :~: (b ': bs) -> a :~: b
headEq Refl = Refl

tailEq :: (a ': as) :~: (b ': bs) -> as :~: bs
tailEq Refl = Refl

concatEq :: as :~: bs -> as' :~: bs' -> (as ++ as') :~: (bs ++ bs')
concatEq Refl Refl = Refl

lowerExpr :: Sing c -> X.Expr a (Keys c) -> S.Expr a c
-- TODO let's implement this first. maybe that helps clarify the let
-- lowering
lowerExpr c = \case
  X.Constructor _ _ _ _ -> error "not implemented"
  X.Accessor _ _ _      -> error "not implemented"
  X.ObjectUpdate _ _ _  -> error "not implemented"
  X.Literal a l         -> S.Literal a $ lowerLiteral c l
  X.Abs a i e           -> lowerAbs c a i e
  X.App a e e'          -> multiApply c a e e'
  X.Var a i p           -> case pLookup'' p c of
                               SomeElemIsElem q n -> S.Var a (STuple2 i n) q
  X.Case _ _ _          -> error "not implemented"
  X.Let _ _ _           -> error "not implemented"

lowerAbs :: Sing c -> a -> Sing i -> X.Expr a (Local i ': Keys c) -> S.Expr a c
lowerAbs c a i e = S.Let a (singletonFunBind a l e') $ f
  where
    l = sing @('PIdent "lambda")
    l' = S.Var a (S.sFunVar $ sLocal l) $ IsElemHead Refl
    (fvvs, e') = multiAbstract' c a (sPure i) e
    -- ^ fvvs are the free variable references, after abstracting over
    -- the explicit lambda binders; they also get abstracted over, so
    -- we need to apply them here, so the resulting expression
    -- contains them freely again.
    f = case fvvs of
          []     -> l'
          v : vs -> S.App a l' $ fmap g (v :| vs)
            where
              g (VarDef i' p) = S.Var a (S.sVarVar i') $ IsElemTail p

singletonBind :: a -> Sing i -> S.Bound a (Snd i) c -> S.BindList a '[i] c
singletonBind a i b = S.BindListCons (S.NonRec a i b) S.BindListNil

singletonFunBind :: a -> Sing t -> S.Abs a c -> S.BindList a '[S.FunVar t] c
singletonFunBind a i f = singletonBind a (S.sFunVar i) $ S.BoundFun f

lowerLiteral :: Sing c -> Literal a X.Expr (Keys c) -> Literal a S.Expr c
lowerLiteral = error "not implemented"

multiApply :: Sing c -> a -> X.Expr a (Keys c) -> X.Expr a (Keys c) -> S.Expr a c
multiApply = go []
  where
    go es c a (X.App _ f f') e' = go (lowerExpr c e' : es) c a f f'
    go es c a e e'              = S.App a (lowerExpr c e) $ (lowerExpr c e') :| es

data VarDef a (c :: [S.Split q]) = forall (i :: q). VarDef (Sing i) (IsElem (S.VarVar i) c)

multiAbstract' :: forall c a is.
                  Sing c -> a
               -> Sing (is :: [PIdent]) -- these are the names we want to abstract over
               -> X.Expr a (Locals is ++ Keys c)
               -> ([VarDef a c],  S.Abs a c)
multiAbstract' c a is = \case
  e -> (_vars, S.Lam a _is' _e')
    where
      e' :: S.Expr a (S.VarVars (Locals is) ++ c)
      e' = case varVarsSection $ sLocals is of
        Refl -> let fstLambda = SLambda @_ @_ @FstSym0 sFst
                in
                  case fmapDist fstLambda (S.sVarVars $ sLocals is) c of
                    Refl -> lowerExpr ((S.sVarVars $ sLocals is) %++ c) e

-- we need to get e' from e, but we the context has to change
-- we have
-- lowerExpr :: Sing c -> X.Expr a (Keys c) -> S.Expr a c
-- Lam :: a -> Sing (is :: NonEmpty k) -> Expr a (VarVars (Locals (ToList is)) ++ FunVars (FunsOnly c)) -> Abs a c
--
-- claim: Locals commutes with Keys
--
-- ansatz:
-- lowerExpr :: Sing (VarVars (Locals (ToList is)) ++ FunVars (FunsOnly c))
--           -> X.Expr a (Keys (VarVars (Locals (ToList is)) ++ FunVars (FunsOnly c)))
--           -> S.Expr a (VarVars (Locals (ToList is)) ++ FunVars (FunsOnly c))
--
-- we need to compute the c :: Sing (VarVars (Locals (ToList is)) ++ FunVars (FunsOnly c))
-- we need a proof that
-- (Keys (VarVars (Locals (ToList is)) ++ FunVars (FunsOnly c)))
-- ~ (Locals (ToList is)) ++ (FunsOnly c)
-- but this will not work.
--
-- 

multiAbstract :: forall c a is. Sing c -> a -> Sing (is :: [PIdent]) -> X.Expr a (Locals is ++ Keys c) -> S.Abs a c
multiAbstract = error "not implemented"

-- multiAbstract :: forall c a is. Sing c -> a -> Sing (is :: [PIdent]) -> X.Expr a (Locals is ++ Keys c) -> S.Abs a c
-- multiAbstract c a is (X.Abs _ i' e') = multiAbstract c a (SCons i' is) e'
-- multiAbstract c a is e               = case (varVarsSection $ sLocals is)
--                                             `concatEq` (funVarsSection $ S.sFunsOnly c)
--                                        of
--                                          Refl -> S.Lam a is e'
--                                                  where
--                                                    e' :: S.Expr a (S.VarVars (Locals is)
--                                                                    ++ S.FunVars (S.FunsOnly c)
--                                                                   )
--                                                    e' = lowerExpr ((S.sVarVars $ sLocals is)
--                                                                    %++ (S.sFunVars $ S.sFunsOnly c)
--                                                                   ) e

-- we need the type equality
-- Keys (Fmap_6989586621679375476 S.VarVarSym0 (Fmap_6989586621679375476 LocalSym0 is)
--   ++ Fmap_6989586621679375476 S.FunVarSym0
--                            (Fmap_6989586621679375476 FstSym0 (Filter (S.Lambda_6989586621679164453Sym1 c) c)
--                            )
--      )
-- ~
-- ~        Locals is ++ Keys c
-- NOTE there seem to be two obstacles here:
-- 1. Keys distributes
-- 2. The operand of lowerExpr isn't always closed wrt variables. We have not taken that into account.
--
-- [X.Expr]
-- Let f = \x -> e
-- in e'
--
-- where
-- y, x ⊢ e
-- y, f ⊢ e'
-- y    ⊢ (the whole expr)
-- ---
-- Let f = ( Let f' = \y x -> e in f' y ) --- ***
-- in e'
--
-- where
-- y, x  ⊢ e
-- y, f' ⊢ f'
-- y, f' ⊢ f' y
-- y, f  ⊢ e'
-- y     ⊢ (the whole expr)
-- --- or else
-- Let f' = \y ... x ... -> e
-- in ( let f = f' y
--      in e'
--    )
-- ---
-- where
-- y, x     ⊢ e
-- y, f'    ⊢ f'
-- y, f'    ⊢ f' y
-- y, f', f ⊢ e'
-- y        ⊢ (the whole expr)
--
-- The first is a funlet, the second is a funlet in a varlet. The
-- third is a varlet in a funlet.

-- Traverse an X.Expr and return the list of free variables.
freeVars :: X.Expr a c -> [Qualified Ident]
freeVars = freeVarsExpr [] -- IDEA we could carry along the list of
                           -- bound variables; avoids the delete
                           -- passes
  where
    values = fmap snd
    freeVarsExpr :: [Qualified Ident] -> X.Expr a c -> [Qualified Ident]
    freeVarsExpr acc = \case
      X.Constructor _ _ _ _ -> acc
      X.Accessor _ _ e -> freeVarsExpr acc e
      X.ObjectUpdate _ e es -> foldl freeVarsExpr (freeVarsExpr acc e) $ values es
      X.Literal _ l -> freeVarsLiteral acc l
        where
          go = foldl freeVarsExpr
          freeVarsLiteral acc' = \case
            ArrayLiteral as  -> go acc' as
            ObjectLiteral fs -> go acc' $ values fs
            _                -> acc'
      X.Abs _ i e -> delete (local $ fromSing i) $ freeVarsExpr acc e
      X.App _ e e' -> freeVarsExpr (freeVarsExpr acc e) e'
      X.Var _ i _ -> (fromSing i) : acc
        -- ^ This is the only place where we can find free variables.
      X.Case _ es as -> foldl freeVarsCaseAlternative (foldl freeVarsExpr acc es) as
        where
          freeVarsCaseAlternative acc'' X.CaseAlternative{..} =
            let vars = case caseAlternativeResult of
                         Left gs -> foldl freeVarsGuards acc'' gs
                         Right e -> freeVarsExpr acc'' e
                freeVarsGuards ac (g, e) = freeVarsExpr (freeVarsExpr ac e) g
                bds = bindersBinderList [] caseAlternativeBinders
                bindersBinderList :: [Qualified Ident] -> X.BinderList a l -> [Qualified Ident]
                bindersBinderList ac X.BinderListNil = ac
                bindersBinderList ac (X.BinderListCons b bs) = bindersBinder (bindersBinderList ac bs) b
                bindersBinder :: [Qualified Ident] -> X.Binder a l -> [Qualified Ident]
                bindersBinder ac = \case
                  X.NullBinder _ -> ac
                  X.LiteralBinder _ b -> bindersLitBinder ac b
                  X.VarBinder _ i -> (local $ fromSing i) : ac
                  X.ConstructorBinder _ _ _ bs -> bindersBinderList ac bs
                  X.NamedBinder _ i b -> (local $ fromSing i) : (bindersBinder ac b)
                bindersLitBinder :: [Qualified Ident] -> X.LitBinder a l -> [Qualified Ident]
                bindersLitBinder ac = \case
                  X.ArrayLitBinder ar  -> bindersLitBinderArray ac ar
                  X.ObjectLitBinder ob -> bindersLitBinderObject ac ob
                  _                    -> ac
                bindersLitBinderArray :: [Qualified Ident] -> X.LitBinderArray a l -> [Qualified Ident]
                bindersLitBinderArray ac = \case
                  X.LitBinderArrayNil -> ac
                  X.LitBinderArrayCons b bs -> bindersBinder (bindersLitBinderArray ac bs) b
                bindersLitBinderObject :: [Qualified Ident] -> X.LitBinderObject a l -> [Qualified Ident]
                bindersLitBinderObject ac = \case
                  X.LitBinderObjectNil -> ac
                  X.LitBinderObjectCons _ b ob -> bindersBinder (bindersLitBinderObject ac ob) b
            in vars \\ bds
      X.Let _ bl e -> let vars = freeVarsExpr acc e
                          bds = binderBindList [] bl
                          binderBindList :: [Qualified Ident] -> X.BindList a l c -> [Qualified Ident]
                          binderBindList ac = \case
                            X.BindListNil         -> ac
                            X.BindListCons _b _bs -> error "not implemented"
                      in
                        vars \\ bds

data SomeBind a c = forall l. SomeBind (S.Bind a l c)

-- lowerBind :: Sing c -> X.Bind a l (Keys c) -> (IsSubList l (Keys l'), S.Bind a l' c)

lowerBind :: Sing c -> X.Bind a l (Keys c) -> SomeBind a c
lowerBind = error "not implemented"
-- lowerBind c = \case
--   X.NonRec a i (X.Constructor a' t n fs) -> SomeBind $ error "not implemented"
--   X.NonRec a i (X.Abs a' i' e) -> SomeBind $ S.NonRec a (S.sFunVar i) $
--                                   S.BoundFun $ multiAbstract c a' (sPure i') e
--                                   -- branch on whether e is closed or
--                                   -- not; also recurse if e is another
--                                   -- abs
--   X.NonRec a i e -> SomeBind $ S.NonRec a (S.sVarVar i) $ S.BoundVar $ lowerExpr c e
--   X.Rec _        -> error "not implemented"

-- class LocalNamesFromRecList l where
--   localNamesFromRecList :: X.RecList a l c -> Sing l

-- instance LocalNamesFromRecList '[] where
--   localNamesFromRecList X.RecNil = SNil

-- instance LocalNamesFromRecList ls => LocalNamesFromRecList (l ': ls) where
--   localNamesFromRecList (X.RecCons _ i _ rl') = SCons i $ localNamesFromRecList rl'

-- getLocalNames :: X.Bind a l c -> Sing l
-- getLocalNames = \case
--   X.NonRec _ i _ -> sPure i
--   X.Rec rl -> localNamesFromRecList rl

data SomeBindList a c = forall l. SomeBindList (S.BindList a l c)

lowerBindList :: Sing c
              -> X.BindList a l (Keys c) -- We don't actually care
                                         -- what the bound names are
                                         -- going in
              -> SomeBindList a c
              -- we can't assume we're only binding function
              -- names. that's only true at the top level. we use the
              -- existential because the list of bound names depends on
              -- the traversal, i.e., here we don't know if a given
              -- name will become a funref or a var ref; but we might
              -- also be binding more (or fewer) names than in the
              -- previous stage.
              --
              -- assuming we never bind fewer names we might want to
              -- return a proof to that effect, instead of the
              -- existential

lowerBindList c = \case
  X.BindListNil -> SomeBindList S.BindListNil
  X.BindListCons b bs -> case lowerBindList c bs of
                           SomeBindList bs' -> case (lowerBind c b) of
                             SomeBind b' -> SomeBindList $ S.BindListCons b' bs'
