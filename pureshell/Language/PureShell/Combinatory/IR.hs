{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE ImpredicativeTypes       #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
module Language.PureShell.Combinatory.IR ( Expr(..)
                                         , Literal(..)
                                         , Bind(..)
                                         , Module(..)
                                         , SingletonContext
                                         , GenExprList(..)
                                         , EmptyContext
                                         , ConcatContexts
                                         , genExprListSingle
                                         , Context(..)
                                         , SContext(..)
                                         , genExprListFold
                                         , ExprList
                                         , TopLevelBind
                                         , moduleFold
                                         ) where

import           Data.Eq.Singletons
import           Data.List.Singletons
import           Data.Singletons
import           Data.Singletons.TH   (genSingletons)
import           Data.Text            (Text)
import           Data.Type.Bool       (If)

newtype Context ids = Context { unContext :: [ids] } deriving (Eq, Show, Ord)

instance PEq (Context ids) where

genSingletons [''Context]

type EmptyContext = 'Context '[]

emptyContext :: Context a
emptyContext = Context []

type SingletonContext s = 'Context '[s] -- TODO figure out how to get this using function promotion

-- TODO we also need a way to actually construct singleton values of these
singletonContext :: ids -> Context ids
singletonContext f = Context [f]

type family ConcatContexts x y where
  ConcatContexts ('Context a) ('Context b) = 'Context (a ++ b)

type a :<> b = ConcatContexts a b
-- can we use a lifted monoid instance here?

type ExprList ids c = GenExprList ids (Expr ids) c

data GenExprList ids (t :: Context ids -> *) (c :: Context ids) where
  GenExprListNil  :: GenExprList ids t EmptyContext
  GenExprListCons :: t c -> GenExprList ids t d -> GenExprList ids t (c :<> d)

genExprListSingle :: t c -> GenExprList ids t (c :<> EmptyContext)
genExprListSingle e = GenExprListCons e GenExprListNil

genExprListFoldl :: (forall c . b -> t c -> b) -> b -> GenExprList ids t d -> b
genExprListFoldl _ b GenExprListNil         = b
genExprListFoldl f b (GenExprListCons e es) = f (genExprListFoldl f b es) e

genExprListFold :: (Monoid b) => (forall c . b -> t c -> b) -> GenExprList ids t d -> b
genExprListFold f = genExprListFoldl f mempty

type family FlattenContextList (cs :: [Context ids]) :: Context ids where
  -- TODO we should be able to define this as a lift of a term level
  -- function
 FlattenContextList '[]       = 'Context '[]
 FlattenContextList '[c]      = c
 FlattenContextList (c ': cs) = ConcatContexts c (FlattenContextList cs)

type family SymbolIsContained s c where
  SymbolIsContained s ('Context '[])    = 'False -- TODO throw a custom type error
  SymbolIsContained s ('Context (t:ts)) = Or [s == t, SymbolIsContained s ('Context ts)]

type family ContextIsContained c d where
  ContextIsContained ('Context '[]) _    = 'True
  ContextIsContained ('Context (s:ss)) d = And [SymbolIsContained s d, ContextIsContained ('Context ss) d]

type family RemoveFromContext s c where
  RemoveFromContext s ('Context '[])       = 'Context '[]
  RemoveFromContext s ('Context (n ': ns)) = ConcatContexts (If (s == n) EmptyContext (SingletonContext n))
                                             (RemoveFromContext s ('Context ns))

data ObjectRow ids c where
  ObjectRow :: ids -> Expr ids c -> ObjectRow ids c

type ObjectRows ids c = GenExprList ids (ObjectRow ids) c

data Literal ids c where
  NumericLiteral :: Either Integer Double -> Literal ids EmptyContext
  StringLiteral  :: Text -> Literal ids EmptyContext
  BooleanLiteral :: Bool  -> Literal ids EmptyContext
  ArrayLiteral   :: ExprList ids c -> Literal ids c
  ObjectLiteral  :: ObjectRows ids c -> Literal ids c

-- | This is the heart of this module
data Expr ids (c :: Context ids) where -- TODO add kind sigs
  Var  :: Sing (s :: ids) -> Expr ids (SingletonContext s)
  Lit  :: Literal ids c -> Expr ids c -- TODO parametrize over the contained literals
  -- this should closely match corefn literals
  App  :: Expr ids c -> ExprList ids d -> Expr ids (c :<> d)
  -- ^ Application of multiple terms
  Abs  :: ((ContextIsContained d c) ~ 'True)
       => Sing (c :: Context ids) -> Expr ids d -> Expr ids EmptyContext
  -- ^ The constraint ensures that all free variables of the expression are bound
  Prim :: String -> Expr ids EmptyContext -- TODO add Prims to the context
  -- ^ A primitive function symbol
  Let :: Bind ids s c -> Expr ids d -> Expr ids (c :<> (RemoveFromContext s d))
-- Case a [Expr a] [CaseAlternative a] -- we definitely need case expressions

-- -- TODO these are nice to have
-- -- Constructor a (ProperName 'TypeName) (ProperName 'ConstructorName) [Ident]
-- -- Accessor a PSString (Expr a)
-- -- ObjectUpdate a (Expr a) [(PSString, Expr a)]

-- -- TODO write a function
-- -- lowerToProcedural :: Expr 'EmptyContext -> Module
-- --
-- -- this will require intermediate functions manipulating state, e.g.,
-- -- varnames and function names, and writing sequencial function
-- -- definitions arising from nested lambda abstractions.
-- --
-- -- actually, we should also define a type for corefn modules. that way
-- -- we can even encode constraints about definedness of imported and
-- -- foreign functions

type TopLevelBind ids s = Bind ids s EmptyContext

data Bind ids s c where
  Bind :: Sing (s :: ids) -> Expr ids c -> Bind ids s c

data Module ids (ss :: [ids]) where
  ModuleNil :: Module ids '[]
  ModuleCons :: TopLevelBind ids s -> Module ids ss -> Module ids (s ': ss)

singletonModule :: TopLevelBind ids s -> Module ids '[s]
singletonModule b = ModuleCons b ModuleNil

moduleFoldl :: forall a ids (ss :: [ids]) . (forall (s :: ids) . a -> TopLevelBind ids s -> a) -> a -> Module ids ss -> a
moduleFoldl _ a ModuleNil        = a
moduleFoldl f a (ModuleCons b m) = f (moduleFoldl f a m) b

moduleFold :: forall a ids (ss :: [ids]) . (Monoid a) => (forall (s :: ids) . a -> TopLevelBind ids s -> a) -> Module ids ss -> a
moduleFold f m = moduleFoldl f mempty m

