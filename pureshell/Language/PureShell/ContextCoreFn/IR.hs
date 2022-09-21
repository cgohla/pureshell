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
module Language.PureShell.ContextCoreFn.IR where

import           Data.Bool.Singletons
import           Data.Eq.Singletons
import           Data.Functor.Singletons
import           Data.Functor.Singletons           (FmapSym0, sFmap)
import           Data.Kind                         (Type)
import           Data.List.Props                   (IsElem (..), decideIsElem)
import           Data.List.Singletons
import           Data.Maybe.Singletons             (JustSym0, NothingSym0,
                                                    SMaybe (..))
import           Data.Ord.Singletons
import           Data.Singletons                   (Sing, SingI, sing)
import           Data.Singletons.Base.TH           (FromInteger, FromString)
import           Data.Singletons.Decide            ((:~:) (..))
import           Data.Singletons.TH                (genSingletons, promote,
                                                    singDecideInstances,
                                                    singletons)
import           Data.Singletons.TH.Options        (defaultOptions,
                                                    defunctionalizedName,
                                                    promotedDataTypeOrConName,
                                                    withOptions)
import           Data.Text                         (Text)
import           GHC.TypeLits                      (Nat)
import           GHC.TypeLits.Singletons           (Symbol)
import           Language.Haskell.TH               (Name)
import           Language.PureScript.AST.SourcePos (SourceSpan)
import           Language.PureScript.Comments      (Comment)
import qualified Language.PureScript.Names         as F (ProperName,
                                                         ProperNameType (..),
                                                         Qualified (..))
import           Language.PureScript.PSString      (PSString)
import           Text.Show.Singletons              ()

-- | Like CoreFn, but with context annotations

data InternalIdentData
  = RuntimeLazyFactory
  | Lazy !Text
--   deriving (Show, Eq, Ord)

-- | Unfortunately we can not use Language.PureScript.Names.ModuleName
-- here because in order to generate SDecide instances, we must be
-- able to derive our own Eq instance in the TH splice below.
newtype ModuleName = ModuleName Text
-- | We need this custom promoted version ModuleName. The `P` prefixed
-- names are needed unfortunately to make the promotion hack below
-- work.
newtype PModuleName = PModuleName Symbol
--   deriving (Ord, Eq, Show)

data Imported a = Imported ModuleName a
data PImported a = PImported PModuleName a

data Qualified a = Qualified (Maybe ModuleName) a
--  deriving (Ord, Eq, Show)
data PQualified a = PQualified (Maybe PModuleName) a
--  deriving (Ord, Eq, Show)

type PQIdent = PQualified PIdent

-- | We can't reuse the def'n from Language.PureScript.Names because
-- that uses Integers instead of Nats.
data Ident
   = Ident Text
   | GenIdent (Maybe Text) Nat
   | UnusedIdent
--   deriving (Ord, Eq, Show)

data PIdent
   = PIdent Symbol
   | PGenIdent (Maybe Symbol) Nat
   | PUnusedIdent
--   deriving (Ord, Eq, Show)

$(let
     customPromote :: Name -> Name
     customPromote n
       | n == ''Imported = ''PImported
       | n == 'Imported = 'PImported
       | n == ''Ident  = ''PIdent
       | n == 'Ident = 'PIdent
       | n == 'GenIdent = 'PGenIdent
       | n == 'UnusedIdent = 'PUnusedIdent
       | n == ''Text     = ''Symbol
       | n == ''ModuleName = ''PModuleName
       | n == 'ModuleName = 'PModuleName
       | n == ''Qualified = ''PQualified
       | n == 'Qualified = 'PQualified
       | otherwise       = promotedDataTypeOrConName defaultOptions n

     customDefun :: Name -> Int -> Name
     customDefun n sat = defunctionalizedName defaultOptions (customPromote n) sat
 in
    withOptions defaultOptions{ promotedDataTypeOrConName = customPromote
                              , defunctionalizedName      = customDefun
                              } $
    do
      let ts = [''Ident, ''ModuleName, ''Qualified, ''Imported]
      a <- genSingletons ts
      c <- singletons [d|
                        local :: Ident -> Qualified Ident
                        local = Qualified Nothing
                        locals :: [Ident] -> [Qualified Ident]
                        locals = fmap local
                        import_ :: Imported a -> Qualified a
                        import_ (Imported m i) = Qualified (Just m) i
                        imports :: [Imported a] -> [Qualified a]
                        imports = fmap import_

                        deriving instance Eq Ident
                        deriving instance Eq ModuleName
                        deriving instance (Eq a) => Eq (Qualified a)
                        deriving instance (Eq a) => Eq (Imported a)

                        -- TODO awaiting solution of
                        -- https://github.com/goldfirere/singletons/issues/538
                        -- deriving instance Show Ident
                        -- deriving instance Show ModuleName
                        -- deriving instance (Show a) => Show (Qualified a)
                        -- deriving instance (Show a) => Show (Imported a)

                        deriving instance Ord Ident
                        deriving instance Ord ModuleName
                        deriving instance (Ord a) => Ord (Qualified a)
                        deriving instance (Ord a) => Ord (Imported a)

                        deriving instance Functor Qualified
                        deriving instance Functor Imported

                        |]
      return $ a <> c
 )

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
data LitBinder a (l :: [PIdent]) where
   NumericLitBinder :: (Either Integer Double) -> LitBinder a '[]
   StringLitBinder  :: PSString -> LitBinder a '[]
   CharLitBinder    :: Char -> LitBinder a '[]
   BooleanLitBinder :: Bool -> LitBinder a '[]
   ArrayLitBinder   :: LitBinderArray a l -> LitBinder a l
   ObjectLitBinder  :: LitBinderObject a l -> LitBinder a l

type TypeName = (F.Qualified (F.ProperName 'F.TypeName))
type ConstructorName = (F.Qualified (F.ProperName 'F.ConstructorName))

data Binder a (l :: [PIdent]) where
 NullBinder        :: a -> Binder a '[]
 LiteralBinder     :: a -> LitBinder a l -> Binder a l
 -- ^ TODO It's not clear this is correct
 VarBinder         :: a -> Sing (i :: PIdent) -> Binder a '[i]
 ConstructorBinder :: a -> TypeName -> ConstructorName -> BinderList a l -> Binder a l
 NamedBinder       :: a -> Sing (i :: PIdent) -> Binder a l -> Binder a (i ': l)

data BinderList a (l :: [PIdent]) where
  BinderListNil :: BinderList a '[]
  BinderListCons :: Binder a l -> BinderList a l' -> BinderList a (l ++ l')

data CaseAlternative a c = forall (l :: [PIdent]). CaseAlternative
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
