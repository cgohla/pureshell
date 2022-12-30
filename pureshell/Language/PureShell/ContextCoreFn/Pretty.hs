{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Language.PureShell.ContextCoreFn.Pretty where

import           Control.Monad.Reader.Class          (MonadReader, asks)
import           Data.List.Singletons                ((%++), SList (SCons))
import           Data.Singletons                     (Sing, SingI (sing),
                                                      SingKind (fromSing))
import           Data.Text                           (unpack)
import           Text.PrettyPrint.ANSI.Leijen        (Doc, char, dot, double,
                                                      dquotes, equals,
                                                      fillBreak, hsep, indent,
                                                      integer, list, parens,
                                                      punctuate, semiBraces,
                                                      squotes, text, tupled,
                                                      vcat, vsep, (<$$>), (<+>))

import           Control.Monad.Reader                (runReader)
import           Language.PureShell.ContextCoreFn.IR (PQIdent, sLocals, sLocal, sImports)
import qualified Language.PureShell.ContextCoreFn.IR as X (Bind (..),
                                                           BindList (..),
                                                           Binder (..),
                                                           BinderList (..),
                                                           CaseAlternative (..),
                                                           Expr (..),
                                                           Ident (..),
                                                           LitBinder (..),
                                                           LitBinderArray (..),
                                                           LitBinderObject (LitBinderObjectCons, LitBinderObjectNil),
                                                           Literal (..),
                                                           Module (..),
                                                           ModuleName (..),
                                                           Qualified (Qualified),
                                                           RecList (..))
import qualified Language.PureShell.CoreFn.IR        as F (ProperName (..),
                                                           Qualified (..))
import Language.PureShell.ContextCoreFn.CodeGen (getBindListVars, getRecListVars, getBinderListVars)

data PrettyOptions c = PrettyOptions { showContext :: Bool
                                     , context     :: Sing (c :: [PQIdent])
                                     }

type MonadPretty c m = (MonadReader (PrettyOptions c) m)

type ShowContext = Bool

prettyModule :: ShowContext -> X.Module a -> Doc
prettyModule s X.Module{..} =
  let
    intro = hsep [text "module", text $ unpack $ X.getModuleName moduleName, text "where"]
    ds = runReader (prettyBindList moduleDecls) $ PrettyOptions s $ sImports moduleImports %++ sLocals moduleForeign
  in
  vsep (intro : ds)

prettyNumericLit :: Either Integer Double -> Doc
prettyNumericLit = \case
  Left i  -> integer i
  Right d -> double d

prettyLiteral :: (MonadPretty c m) => X.Literal a c -> m Doc
prettyLiteral = \case
  X.NumericLiteral n -> pure $ prettyNumericLit n
  X.StringLiteral s  -> pure $ dquotes $ text $ show s
  X.CharLiteral c    -> pure $ squotes $ char c
  X.BooleanLiteral b -> pure $ text $ show b
  X.ArrayLiteral as  -> list <$> traverse prettyExpr as
  X.ObjectLiteral o  -> semiBraces <$> traverse (\(i, e) -> assign (text $ show i) <$> (prettyExpr e)) o

prettyIdent :: X.Ident -> Doc
prettyIdent = \case
  X.Ident t      -> text $ unpack t
  X.GenIdent m n -> (maybe mempty (text . unpack) m) <> (text $ show n)
  X.UnusedIdent  -> text "UNUSED"

prettyModuleMame :: X.ModuleName -> Doc
prettyModuleMame = (text. unpack . X.getModuleName)

prettyQualifiedIdent :: X.Qualified X.Ident -> Doc
prettyQualifiedIdent (X.Qualified m i) = mconcat $ punctuate dot $ q <> [prettyIdent i]
  where
    q = maybe mempty (pure . prettyModuleMame) m

prettyLitArrayBinder :: X.LitBinderArray a l -> [Doc]
prettyLitArrayBinder = \case
  X.LitBinderArrayNil       -> mempty
  X.LitBinderArrayCons b bs -> prettyBinder b : prettyLitArrayBinder bs

prettyLitBinderObject :: X.LitBinderObject a l -> [Doc]
prettyLitBinderObject = \case
  X.LitBinderObjectNil -> mempty
  X.LitBinderObjectCons f b o -> assign (text $ show f) (prettyBinder b) : prettyLitBinderObject o

prettyLitBinder :: X.LitBinder a l -> Doc
prettyLitBinder = \case
  X.NumericLitBinder n -> prettyNumericLit n
  X.StringLitBinder s  -> dquotes $ text $ show s
  X.CharLitBinder c    -> squotes $ char c
  X.BooleanLitBinder b -> text $ show b
  X.ArrayLitBinder as  -> list $ prettyLitArrayBinder as
  X.ObjectLitBinder o  -> semiBraces $ prettyLitBinderObject o

prettyProperQualifiedName :: F.Qualified (F.ProperName n) -> Doc
prettyProperQualifiedName = \case
  F.Qualified _q n -> prettyProperName n -- NOTE we are ignoring the qualifier

prettyBinder :: X.Binder a l -> Doc
prettyBinder = \case
  X.NullBinder a                 -> text "_"
  X.LiteralBinder a l            -> prettyLitBinder l
  X.VarBinder a i                -> prettyIdent $ fromSing i
  X.ConstructorBinder a tn cn bs -> mconcat [ prettyProperQualifiedName tn
                                            , char '.'
                                            , prettyProperQualifiedName cn
                                            , semiBraces $ prettyBinderList bs
                                            ]
  X.NamedBinder a i b            -> mconcat [ text $ show $ fromSing i
                                            , char '@'
                                            , parens $ prettyBinder b
                                            ]

prettyBinderList :: X.BinderList a l -> [Doc]
prettyBinderList = \case
  X.BinderListNil       -> mempty
  X.BinderListCons b bs -> prettyBinder b : prettyBinderList bs

rightarrow :: Doc
rightarrow = text "->"

prettyCaseAlternative :: (MonadPretty c m) => X.CaseAlternative a c-> m Doc
prettyCaseAlternative (X.CaseAlternative bs rs) = do
  rs' <- case rs of
    Left gs -> do
      p <- asks showContext
      c <- asks context
      let h = vcat <$> traverse (\(g, e) -> hsep <$> sequenceA [prettyExpr g, pure $ char '|', prettyExpr e]) gs
      pure $ runReader h $ PrettyOptions p $ (sLocals $ getBinderListVars bs) %++ c
    Right e -> do
      p <- asks showContext
      c <- asks context
      pure $ runReader (prettyExpr e) $ PrettyOptions p $ (sLocals $ getBinderListVars bs) %++ c

  pure $ fillBreak 8 (tupled $ prettyBinderList bs) <+> rightarrow <+> rs'

prettyRecList :: (MonadPretty c m) => X.RecList a l c -> m [Doc]
prettyRecList = \case
  X.RecNil           -> pure mempty
  X.RecCons a i e rs -> (:) <$> (assign (prettyIdent $ fromSing i) <$> (prettyExpr e)) <*> (prettyRecList rs)

prettyBind :: (MonadPretty c m) => X.Bind a l c -> m Doc
prettyBind = \case
  X.NonRec a i e -> assign (prettyIdent $ fromSing i) <$> prettyExpr e
  X.Rec bs       -> do
    p <- asks showContext
    c <- asks context
    pure $ runReader (vcat <$> prettyRecList bs) $ PrettyOptions p $ (sLocals $ getRecListVars bs) %++ c
    -- ^ TODO this pattern occurs at least three times, so maybe factor
    -- it out

prettyBindList :: (MonadPretty c m) => X.BindList a l c -> m [Doc]
prettyBindList = \case
  X.BindListNil       -> pure mempty
  X.BindListCons b bs -> (:) <$> prettyBind b <*> prettyBindList bs

assign :: Doc -> Doc -> Doc
assign l r = l <+> equals <+> r

prettyProperName :: F.ProperName n -> Doc
prettyProperName = text . unpack . F.runProperName

wrapContext :: forall c m. (MonadPretty c m) => Doc -> m Doc
wrapContext d = do
  o <- asks showContext
  c <- asks context
  pure $ if o then
           parens $ hsep [ list $ fmap prettyQualifiedIdent $ fromSing c
                         , char '⊢'
                         , d
                         ]
         else
           d

prettyExpr :: forall m a c. (MonadPretty c m) => X.Expr a c -> m Doc
prettyExpr = \case
  X.Constructor _a tn cn fs -> pure $ mconcat [ prettyProperName tn
                                             , char '.'
                                             , prettyProperName cn
                                             , semiBraces $ fmap prettyIdent $ fromSing fs
                                             ]
  X.Accessor _a fn e -> do
    e' <- prettyExpr e
    wrapContext $ mconcat [ parens e'
                          , char '.'
                          , text $ show fn
                          ]
  X.ObjectUpdate _a e as -> do
    e' <- prettyExpr e
    as' <- semiBraces <$> (traverse (\(f, g) -> assign (text $ show f) <$> prettyExpr g) $ as)
    wrapContext $ parens $ e' <+> as'
  X.Literal _a l -> prettyLiteral l
  X.Abs _a i e -> do
    p <- asks showContext
    c <- asks context
    let
      ifNested :: r -> r -> r
      ifNested f g  = case e of
                        X.Abs _ _ _-> f
                        _ -> g
    f <- pure $ runReader (ifNested pure (fmap parens . wrapContext) =<< prettyExpr e) $ PrettyOptions p $ SCons (sLocal i) c
    pure $ char 'λ' <> (prettyIdent $ fromSing i) <+> rightarrow <+> f
  X.App _a e e' -> do
    f <- prettyExpr e
    f' <- prettyExpr e'
    pure $ f <+> f' -- TODO parenthesise the f' if it is also an App
  X.Var _a i _p ->
    pure $ prettyQualifiedIdent $ fromSing i
  X.Case _a es as -> do
    es' <- traverse prettyExpr es
    as' <- traverse prettyCaseAlternative as
    pure $ hsep [ text "case"
                , tupled es'
                , text "of"
                ]
      <$$> indent 4 (vcat as')
  X.Let _a bs e -> do
    l <- prettyBindList bs
    p <- asks showContext
    c <- asks context
    e' <- pure $ runReader (wrapContext =<< prettyExpr e) $ PrettyOptions p $ (sLocals $ getBindListVars bs) %++ c
    pure $ vcat [ text "let"
                , indent 4 $ vcat l
                , text "in"
                , indent 4 e'
                ]
