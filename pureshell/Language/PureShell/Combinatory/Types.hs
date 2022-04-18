{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE ImpredicativeTypes       #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
module Language.PureShell.Combinatory.Types where

import           Data.ByteString              (ByteString)
import           Data.Singletons
import           Data.Singletons.Prelude.Eq
import           Data.Singletons.Prelude.List
import           Data.Singletons.TH           (genSingletons)
import           Data.Text                    (Text)
import           GHC.TypeLits                 (Symbol)
import           GHC.Word                     (Word8)

data OnlyByteStrings

data Foo = Foo1 | Foo2 | Bar1 | Bar3 | Boom4  deriving (Eq, Show, Ord)

instance PEq Foo where

genSingletons [''Foo]

newtype Context = Context { unContext :: [Foo] } deriving (Eq, Show, Ord)-- we really need to figure out how to handle strings / chars here.

-- wow, type synonyms are not handled transparently by singletons

genSingletons [''Context]

type EmptyContext = 'Context '[]

type SingletonContext s = 'Context '[s] -- TODO figure out how to get this using function promotion

-- TODO we also need a way to actually construct singleton values of these
singletonContext :: Foo -> Context
singletonContext f = Context [f]

type family ConcatContexts x y where
  ConcatContexts ('Context a) ('Context b) = 'Context (a ++ b)
  -- can we use a lifted monoid instance here?

type ExprList c = GenExprList Expr c

data GenExprList (t :: Context -> *) (c :: Context) where
  GenExprListNil  :: GenExprList t EmptyContext
  GenExprListCons :: t c -> GenExprList t d -> GenExprList t (ConcatContexts c d)

genExprListSingle :: t c -> GenExprList t (ConcatContexts c EmptyContext)
genExprListSingle e = GenExprListCons e GenExprListNil

genExprListFoldl :: (forall c . b -> t c -> b) -> b -> GenExprList t d -> b
genExprListFoldl _ b GenExprListNil         = b
genExprListFoldl f b (GenExprListCons e es) = f (genExprListFoldl f b es) e

genExprListFold :: (Monoid b) => (forall c . b -> t c -> b) -> GenExprList t d -> b
genExprListFold f = genExprListFoldl f mempty

type family FlattenContextList (cs :: [Context]) :: Context where
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

-- data Bind = Bind Foo (Expr c) deriving (Show, Eq, Ord)

data ObjectRow c where
  ObjectRow :: Foo -> Expr c -> ObjectRow c

type ObjectRows c = GenExprList ObjectRow c

data Literal c where
  NumericLiteral :: Either Integer Double -> Literal EmptyContext
  StringLiteral  :: String -> Literal EmptyContext -- We may need to refine this
  BooleanLiteral :: Bool  -> Literal EmptyContext
  ArrayLiteral   :: ExprList c -> Literal c
  ObjectLiteral  :: ObjectRows c -> Literal c

-- | This is the heart of this module
data Expr (c :: Context) where -- TODO add kind sigs
  Var  :: Sing s -> Expr (SingletonContext s)
  Lit  :: Literal c -> Expr c -- TODO parametrize over the contained literals
  -- this should closely match corefn literals
  App  :: Expr c -> ExprList d -> Expr (ConcatContexts c d)
  -- ^ Application of multiple terms
  Abs  :: ((ContextIsContained d c) ~ 'True) => Sing c -> Expr d -> Expr EmptyContext
  -- ^ The constraint ensures that all free variables of the expression are bound
  Prim :: String -> Expr EmptyContext -- TODO add Prims to the context
  -- ^ A primitive function symbol
-- Let :: Bind s  -> Expr c -> Expr (Singleton) -- We need let bindings
-- Case a [Expr a] [CaseAlternative a] -- we definitely need case expressions

-- TODO these are nice to have
-- Constructor a (ProperName 'TypeName) (ProperName 'ConstructorName) [Ident]
-- Accessor a PSString (Expr a)
-- ObjectUpdate a (Expr a) [(PSString, Expr a)]

-- TODO write a function
-- lowerToProcedural :: Expr 'EmptyContext -> Module
--
-- this will require intermediate functions manipulating state, e.g.,
-- varnames and function names, and writing sequencial function
-- definitions arising from nested lambda abstractions.
--
-- actually, we should also define a type for corefn modules. that way
-- we can even encode constraints about definedness of imported and
-- foreign functions





-- TODO we need to do something about identifier kinds
-- type family Literal x where
--   Literal OnlyByteStrings = ByteString

-- type family Ident x where
--   Ident OnlyByteStrings = ByteString

type TopLevelBindList = GenExprList Bind EmptyContext

type TopLevelBind = Bind EmptyContext

data Bind c where
  Bind :: Foo -> Expr c -> Bind c -- TODO maybe the identifier needs to be Sing'ed later

data Module = TopLevelBindList
