{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE ExplicitNamespaces       #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE IncoherentInstances      #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverlappingInstances     #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
module Language.PureShell.Context.CoreFn.IR where

import           Data.Bool.Singletons
import           Data.Kind                         (Type)
import           Data.List.Props                   (IsElem (..), decideIsElem)
import           Data.List.Singletons
import           Data.Singletons                   (sing)
import           Language.PureScript.AST.SourcePos (SourceSpan)
import           Language.PureScript.Comments      (Comment)
import qualified Language.PureScript.Names         as F (ProperName,
                                                         ProperNameType (..),
                                                         Qualified (..))
import           Language.PureScript.PSString      (PSString)
import           Text.Show.Singletons              ()

import           Language.PureShell.Context.Ident    (Imports, Local, Locals,
                                                      ModuleName, PIdent (..),
                                                      PImported, PQIdent,
                                                      PQualified(..))

-- | Like CoreFn, but with context annotations

data Literal a c
   = NumericLiteral (Either Integer Double)
   | StringLiteral PSString
   | CharLiteral Char
   | BooleanLiteral Bool
   | ArrayLiteral [Expr a c]
   | ObjectLiteral [(PSString, Expr a c)]

data Expr a (c :: [PQIdent]) where
  Constructor  :: a -> (F.ProperName 'F.TypeName) -> (F.ProperName 'F.ConstructorName) -> Sing (fs :: [PIdent]) -> Expr a c
  -- ^ This is like a lambda
  Accessor     :: a -> PSString -> (Expr a c) -> Expr a c -- ^ This is like an application
  ObjectUpdate :: a -> (Expr a c) -> [(PSString, Expr a c)] -> Expr a c -- ^ This is like an application
  Literal      :: a -> Literal a c -> Expr a c
  Abs          :: a -> Sing (i :: PIdent) -> (Expr a ((Local i) ': c)) -> Expr a c
  App          :: a -> Expr a c -> Expr a c -> Expr a c
  Var          :: a -> Sing (i :: PQIdent) -> IsElem i c -> Expr a c
  Case         :: a -> [Expr a c] -> [CaseAlternative a c] -> Expr a c
  -- ^ NOTE As is this does not guarantee that there are enough
  -- expressions for the binders in each branch
  Let          :: a -> BindList a l c -> {- in -} Expr a ((Locals l) ++ c) -> Expr a c

data BindList a (l :: [PIdent]) (c :: [PQIdent]) where
  BindListNil :: BindList a '[] c
  BindListCons :: Bind a l c -> BindList a l' c -> BindList a (l ++ l') c
  -- NOTE We are not propagating the l into the tail of the list. This
  -- could be wrong; CoreFn might assume scope nesting here.

data Bind a (l :: [PIdent]) (c :: [PQIdent]) where
   NonRec :: a -> (Sing (i :: PIdent)) {- = -} -> (Expr a c) -> Bind a '[i] c
   Rec    :: RecList a l ((Locals l) ++ c) -> Bind a l c

-- | We track the list of names bound in a recursive let block in the
-- type parameter l. To make those names available inside all bound
-- expressions, l needs to be appended to the context c where RecList
-- is used.
data family RecList a (l :: [PIdent]) (c :: [PQIdent]) :: Type
data instance RecList a '[] c = RecNil
data instance RecList a (i ': is) c = RecCons a (Sing i) {- = -} (Expr a c)
                                      {- : -} (RecList a is c)

type Guard a c = Expr a c

data LitBinderArray a (l :: [PIdent]) where
  LitBinderArrayNil :: LitBinderArray a '[]
  LitBinderArrayCons :: Binder a l {- : -} -> LitBinderArray a l'
                     -> LitBinderArray a (l ++ l')

data LitBinderObject a (l :: [PIdent]) where
  LitBinderObjectNil :: LitBinderObject a '[]
  LitBinderObjectCons :: PSString
                      -> Binder a l {- : -}
                      -> LitBinderObject a l'
                      -> LitBinderObject a (l ++ l')

-- | NOTE we can't reuse the Literal type from CoreFn because we need
-- to accumulate names.
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
 VarBinder         :: a -> Sing (i :: PIdent) -> Binder a '[i]
 ConstructorBinder :: a -> TypeName -> ConstructorName -> BinderList a l -> Binder a l
 NamedBinder       :: a -> Sing (i :: PIdent) -> Binder a l -> Binder a (i ': l)

data BinderList a l where
  BinderListNil :: BinderList a '[]
  BinderListCons :: Binder a l -> BinderList a l' -> BinderList a (l ++ l')

data CaseAlternative a c = forall l. CaseAlternative
   { caseAlternativeBinders :: BinderList a l
   , caseAlternativeResult  :: Either [(Guard a (Locals l ++ c), Expr a (Locals l ++ c))] (Expr a (Locals l ++ c))
   }

example0 :: Expr () '[]
example0 = Let () (BindListCons
                    (Rec (RecCons ()
                          (sing @('PIdent "bar")) {- = -} (Var ()
                                                           (sing @('PQualified 'Nothing ('PIdent "bar")))
                                                           (IsElemHead Refl)
                                                          ) $
                         -- ^ NOTE This is legal in a LetRec. We must
                         -- take care when lowering though: recursion in
                         -- bash must use functions.
                          RecCons ()
                          (sing @('PIdent "foo")) {- = -} (Literal () $ BooleanLiteral True)
                          RecNil
                         )
                    )
                    BindListNil
                  )
           (Var ()
             (sing @('PQualified 'Nothing ('PIdent "foo")))
             (IsElemTail $ IsElemHead Refl
             )
           )

example1 :: Expr () '[ Local('PIdent "foo")]
example1 = Case () [Var () (sing @(Local('PIdent "foo"))) $ IsElemHead Refl ]
           [ {- match -} CaseAlternative (BinderListCons (LiteralBinder () $ BooleanLitBinder False ) BinderListNil)
             {- then -} (Right $ Literal () $ BooleanLiteral True)
           ]

data Module a = forall l f i. Module
  { moduleSourceSpan :: SourceSpan
  , moduleComments   :: [Comment]
  , moduleName       :: ModuleName
  , modulePath       :: FilePath
  -- , moduleImports    :: [(a, ModuleName)]
  -- , moduleExports    :: [Ident]
  -- , moduleReExports  :: Map ModuleName [Ident]
  -- NOTE Since Bash has no notion of modules or name spaces we have
  -- no use for these.
  , moduleImports    :: Sing (i :: [PImported PIdent])
  -- ^ This is an enumeration of all imported symbols, not just module
  -- names as in CoreFn. We will need to compute this information.
  , moduleForeign    :: Sing (f :: [PIdent])
  , moduleDecls      :: BindList a l (Imports i ++ Locals f)
  }
