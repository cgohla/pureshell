{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Language.PureShell.Context.Split.IR where

import           Data.Kind                           (Type)
import           Data.List.Singletons                (-- Lookup, SList (..),
                                                      type (++))
import           Data.Singletons                     (Sing)
import           Data.Singletons.Prelude.List.Props5 (IsElement)
import           Data.Tuple.Singletons               (Snd)
import qualified Language.PureScript.Names           as F (ProperName,
                                                           ProperNameType (..),
                                                           Qualified (..))
import           Language.PureScript.PSString        (PSString)

-- TODO factor out names from Context

-- TODO a lot of types outside of Expr can presumably be shared with
-- ContextCoreFn, if properly polymorphised

data NameType = VarName | FunName

type Ident = ()

type family LocalsSplit (a :: k) :: k'

type SplitIdent = (Ident, NameType)

type Context = [QSplitIdent]

type Qualified a = a

type QSplitIdent = (Qualified Ident, NameType)

data Literal a c
   = NumericLiteral (Either Integer Double)
   | StringLiteral PSString
   | CharLiteral Char
   | BooleanLiteral Bool
   | ArrayLiteral [Expr a c]
   | ObjectLiteral [(PSString, Expr a c)]

data Expr a (c :: [(k', n)]) where
  Accessor     :: a -> PSString -> (Expr a c) -> Expr a c
  ObjectUpdate :: a -> Expr a c -> [(PSString, Expr a c)] -> Expr a c
  Literal      :: a -> Literal a c -> Expr a c
  App          :: a -> Expr a c -> [Expr a c] -> Expr a c
  Var          :: a -> Sing (i :: (k', n)) -> IsElement i c -> Expr a c
  Case         :: a -> [Expr a c] -> [CaseAlternative a c] -> Expr a c
  Let          :: a -> BindList a l c
               -> {- in -} Expr a ((LocalsSplit l) ++ c)
               -> Expr a c

-- | Abstractions are only allowed in let bindings
data Abs a (c :: [(k', n)]) where
  Lam :: a -> Sing (is :: [Ident]) -> Expr a (LocalsSplit is ++ c) -> Abs a c
  Con :: a -> TypeName -> ConstructorName -> Sing (fs :: [Ident]) -> Abs a c

data family Bound a (t :: n) (c :: [(k', n)]) :: Type
data instance Bound a 'VarName c = BoundVar (Expr a c)
data instance Bound a 'FunName c = BoundFun (Abs a c)

data BindList a (l :: [(k, n)]) (c :: [(k', n)]) where
  BindListNil  :: BindList a '[] c
  BindListCons :: Bind a l c -> BindList a l' c -> BindList a (l ++ l') c

data Bind a (l :: [(k, n)]) (c :: [(k', n)]) where
   NonRec :: a -> Sing (i :: (k', n)) {- = -}
          -> Bound a (Snd i) c -> Bind a '[t] c
   Rec    :: RecList a l ((LocalsSplit l) ++ c) -> Bind a l c

data family RecList a (l :: [(k, n)]) (c :: [(k', n)]) :: Type
data instance RecList a '[] c = RecNil
data instance RecList a (i ': is) c = RecCons a (Sing i)
                                      {- = -} (Bound a (Snd i) c)
                                      {- : -} (RecList a is c)

type Guard a c = Expr a c

data LitBinderArray a l where
  LitBinderArrayNil  :: LitBinderArray a '[]
  LitBinderArrayCons :: Binder a l {- : -} -> LitBinderArray a l'
                     -> LitBinderArray a (l ++ l')

data LitBinderObject a l where
  LitBinderObjectNil  :: LitBinderObject a '[]
  LitBinderObjectCons :: PSString
                      -> Binder a l {- : -}
                      -> LitBinderObject a l'
                      -> LitBinderObject a (l ++ l')

data LitBinder a l where
   NumericLitBinder :: (Either Integer Double) -> LitBinder a '[]
   StringLitBinder  :: PSString -> LitBinder a '[]
   CharLitBinder    :: Char -> LitBinder a '[]
   BooleanLitBinder :: Bool -> LitBinder a '[]
   ArrayLitBinder   :: LitBinderArray a l -> LitBinder a l
   ObjectLitBinder  :: LitBinderObject a l -> LitBinder a l

type TypeName = (F.Qualified (F.ProperName 'F.TypeName))
type ConstructorName = (F.Qualified (F.ProperName 'F.ConstructorName))

data Binder a l where
 NullBinder        :: a -> Binder a '[]
 LiteralBinder     :: a -> LitBinder a l -> Binder a l
 -- ^ TODO It's not clear this is correct
 VarBinder         :: a -> Sing i -> Binder a '[i]
 ConstructorBinder :: a -> TypeName -> ConstructorName -> BinderList a l -> Binder a l
 NamedBinder       :: a -> Sing i -> Binder a l -> Binder a (i ': l)

data BinderList a l where
  BinderListNil :: BinderList a '[]
  BinderListCons :: Binder a l -> BinderList a l' -> BinderList a (l ++ l')

data CaseAlternative a c = forall l. CaseAlternative
   { caseAlternativeBinders :: BinderList a l
   , caseAlternativeResult  :: Either [(Guard a (LocalsSplit l ++ c), Expr a (LocalsSplit l ++ c))] (Expr a (LocalsSplit l ++ c))
   }
