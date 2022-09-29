{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE ExplicitNamespaces       #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
module Language.PureShell.Context.Split.IR where

import           Data.Singletons.Decide              (type (:~:) (..))
import           Data.Singletons.TH.Options          (defaultOptions,
                                                      defunctionalizedName,
                                                      promotedDataTypeOrConName,
                                                      withOptions)
import           Data.Text                           (Text)
import           GHC.TypeLits.Singletons             (Symbol)
import           Language.Haskell.TH                 (Name)

import           Data.Bool.Singletons                (FalseSym0, SBool (..),
                                                      TrueSym0)
import           Data.Eq.Singletons
import           Data.Function.Singletons            (type (.@#@$), (%.))
import           Data.Functor.Singletons             (FmapSym0, sFmap)
import           Data.Kind                           (Type)
import           Data.List.NonEmpty                  (NonEmpty (..))
import           Data.List.NonEmpty.Singletons       (SNonEmpty (..),
                                                      type ToList)
import           Data.List.Singletons                (FilterSym0, SList (..),
                                                      sFilter, type (++))
import           Data.Singletons                     (Sing, sing)
import           Data.Singletons.TH                  (singletons)
import           Data.Tuple.Singletons               (FstSym0, STuple2 (..),
                                                      Snd, Tuple2Sym0, sFst)
import           Language.PureScript.AST.SourcePos   (SourceSpan)
import           Language.PureScript.Comments        (Comment)
import qualified Language.PureScript.Names           as F (ProperName,
                                                           ProperNameType (..),
                                                           Qualified (..))
import           Language.PureScript.PSString        (PSString)

import           Data.List.Props                     (IsElem (..))
import           Language.PureShell.Context.Ident    (Ident (..), Imported (..),
                                                      Imports, LocalSym0,
                                                      Locals, ModuleName (..),
                                                      PIdent (..),
                                                      PImported (..),
                                                      PModuleName (..),
                                                      PQualified (..),
                                                      Qualified (..), local,
                                                      sLocal)
import           Language.PureShell.Context.Literals (Literal (..))

-- TODO a lot of types outside of Expr can presumably be shared with
-- ContextCoreFn, if properly polymorphised

$(singletons [d|
               data NameType = VarName | FunName deriving Eq
               |]
 )

type Split a = (a, NameType)

-- TODO factor put the custom name mapping
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
    singletons [d|
                 funVar :: k -> Split k
                 funVar k = (k, FunName)
                 funVars :: [k] -> [Split k]
                 funVars = fmap funVar

                 varVar :: k -> Split k
                 varVar k = (k, VarName)
                 varVars :: [k] -> [Split k]
                 varVars = fmap varVar

                 localSplit :: Split a -> Split (Qualified a)
                 localSplit (k, n) = (local k, n)
                 localSplits :: [Split a] -> [Split (Qualified a)]
                 localSplits = fmap localSplit

                 funsOnly :: [Split a] -> [a]
                 funsOnly = fmap fst . filter (\(_, n) -> n == FunName)
                 |]
 )

-- | Abstractions are only allowed in let bindings
data Abs a (c :: [Split (PQualified k)]) where
  Lam :: a
      -> Sing (is :: NonEmpty k)
      -> Expr a (VarVars (Locals (ToList is)) ++ FunVars (FunsOnly c))
      -> Abs a c
  Con :: a
      -> TypeName
      -> ConstructorName
      -> Sing (fs :: [k])
      -> Abs a c

data Expr a (c :: [Split (PQualified k)]) where
  Accessor     :: a -> PSString -> (Expr a c) -> Expr a c
  ObjectUpdate :: a -> Expr a c -> [(PSString, Expr a c)] -> Expr a c
  Literal      :: a -> Literal a Expr c -> Expr a c
  App          :: a -> Expr a c -> NonEmpty (Expr a c) -> Expr a c
  Var          :: a -> Sing (i :: Split (PQualified k)) -> IsElem i c -> Expr a c
  Case         :: a -> [Expr a c] -> [CaseAlternative a c] -> Expr a c
  Let          :: a -> BindList a l c
               -> {- in -} Expr a ((LocalSplits l) ++ c)
               -> Expr a c

data family Bound a (t :: n) (c :: [Split (PQualified k)]) :: Type
data instance Bound a 'VarName c = BoundVar (Expr a c)
data instance Bound a 'FunName c = BoundFun (Abs a c)

data BindList a (l :: [Split k]) (c :: [Split (PQualified k)]) where
  BindListNil  :: BindList a '[] c -- TODO we should probably not allow empty lists
  BindListCons :: Bind a l c -> BindList a l' c -> BindList a (l ++ l') c

data Bind a (l :: [Split k]) (c :: [Split (PQualified k)]) where
   NonRec :: a -> Sing (i :: Split k) {- = -}
          -> Bound a (Snd i) c -> Bind a '[i] c
   Rec    :: RecList a l ((LocalSplits l) ++ c) -> Bind a l c

data family RecList a (l :: [Split k]) (c :: [Split (PQualified k)]) :: Type
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
   , caseAlternativeResult  :: Either [ ( Guard a (LocalSplits l ++ c)
                                        , Expr a (LocalSplits l ++ c)
                                        )
                                      ]
                               (Expr a (LocalSplits l ++ c))
   }

example0 :: a -> Sing c -> Expr a (c :: [Split (PQualified PIdent)])
example0 a c = Let a (BindListCons
                       (NonRec
                        a
                        (sing @('( 'PIdent "flip", 'FunName)))
                        (BoundFun $ Lam a (sing @('PIdent "f" ':|
                                                  '[ 'PIdent "x"
                                                   , 'PIdent "y"
                                                   ]
                                                 )
                                          ) $
                          let
                            f = sVarVar $ sLocal $ sing @('PIdent "f")
                            x = sVarVar $ sLocal $ sing @('PIdent "x")
                            y = sVarVar $ sLocal $ sing @('PIdent "y")
                          in
                            (App a (Var a f $ IsElemHead Refl) $
                             ( Var a y $ IsElemTail $ IsElemTail $ IsElemHead Refl ) :|
                             [ Var a x $ IsElemTail $ IsElemHead Refl ]
                            )
                        )
                       )
                       BindListNil
                     ) $
               let n = sFunVar $ sLocal $ sing @('PIdent "flip") in
               Var a n $ IsElemHead Refl

data Module a = forall k l f i. Module
  { moduleSourceSpan :: SourceSpan
  , moduleComments   :: [Comment]
  , moduleName       :: ModuleName
  , modulePath       :: FilePath
  -- , moduleImports    :: [(a, ModuleName)]
  -- , moduleExports    :: [Ident]
  -- , moduleReExports  :: Map ModuleName [Ident]
  -- NOTE Since Bash has no notion of modules or name spaces we have
  -- no use for these.
  , moduleImports    :: Sing (i :: [PImported k])
  -- ^ This is an enumeration of all imported symbols, not just module
  -- names as in CoreFn. We will need to compute this information.
  , moduleForeign    :: Sing (f :: [k])
  , moduleDecls      :: BindList a l (FunVars (Imports i ++ Locals f))
  -- ^ NOTE All imports and all foreign symbols are function names
  }
