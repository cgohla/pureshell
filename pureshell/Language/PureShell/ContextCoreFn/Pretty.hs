{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Language.PureShell.ContextCoreFn.Pretty where

import           Data.Singletons                     (SingI,
                                                      SingKind (fromSing))
import           Data.Text                           (unpack)
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
import           Text.PrettyPrint.ANSI.Leijen        (Doc, char, dot, double,
                                                      dquotes, equals,
                                                      fillBreak, hsep, indent,
                                                      integer, list, parens,
                                                      punctuate, semiBraces,
                                                      squotes, text, tupled,
                                                      vcat, vsep, (<$$>), (<+>))

prettyModule :: X.Module a -> Doc
prettyModule X.Module{..} = vsep $ intro : prettyBindList moduleDecls
  where
    intro = hsep [text "module", text $ unpack $ X.getModuleName moduleName, text "where"]

prettyNumericLit :: Either Integer Double -> Doc
prettyNumericLit = \case
  Left i  -> integer i
  Right d -> double d

prettyLiteral :: X.Literal a c -> Doc
prettyLiteral = \case
  X.NumericLiteral n -> prettyNumericLit n
  X.StringLiteral s  -> dquotes $ text $ show s
  X.CharLiteral c    -> squotes $ char c
  X.BooleanLiteral b -> text $ show b
  X.ArrayLiteral as  -> list $ fmap prettyExpr as
  X.ObjectLiteral o  -> semiBraces $ fmap (\(i, e) -> assign (text $ show i) (prettyExpr e)) o

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

prettyCaseAlternative :: X.CaseAlternative a c-> Doc
prettyCaseAlternative (X.CaseAlternative bs rs) = fillBreak 8 (tupled $ prettyBinderList bs) <+> rightarrow <+> rs'
  where
    rs' = case rs of
      Left gs -> vcat $ fmap (\(g, e) -> prettyExpr g <+> char '|' <+> prettyExpr e) gs
      Right e -> prettyExpr e

prettyRecList :: X.RecList a l c -> [Doc]
prettyRecList = \case
  X.RecNil           -> mempty
  X.RecCons a i e rs -> assign (prettyIdent $ fromSing i) (prettyExpr e) : prettyRecList rs

prettyBind :: X.Bind a l c -> Doc
prettyBind = \case
  X.NonRec a i e -> assign (prettyIdent $ fromSing i) $ prettyExpr e
  X.Rec bs       -> vcat $ prettyRecList bs

prettyBindList :: X.BindList a l c -> [Doc]
prettyBindList = \case
  X.BindListNil       -> mempty
  X.BindListCons b bs -> prettyBind b : prettyBindList bs

assign :: Doc -> Doc -> Doc
assign l r = l <+> equals <+> r

prettyProperName :: F.ProperName n -> Doc
prettyProperName = text . unpack . F.runProperName

prettyExpr :: X.Expr a c -> Doc
prettyExpr = \case
  X.Constructor a tn cn fs -> mconcat [ prettyProperName tn
                                      , char '.'
                                      , prettyProperName cn
                                      , semiBraces $ fmap prettyIdent $ fromSing fs
                                      ]
  X.Accessor a fn e -> mconcat [ parens $ prettyExpr e
                               , char '.'
                               , text $ show fn
                               ]
  X.ObjectUpdate a e as -> parens $ prettyExpr e
                           <+> (semiBraces $ fmap (\(f, e') -> assign (text $ show f) $ prettyExpr e') $ as)
  X.Literal a l -> prettyLiteral l
  X.Abs a i e -> parens $ char 'λ' <> (prettyIdent $ fromSing i) <+> rightarrow <+> prettyExpr e
  X.App a e e' -> prettyExpr e <+> (parens $ prettyExpr e')
  X.Var a i _p -> prettyQualifiedIdent $ fromSing i
  X.Case a es as -> hsep [ text "case"
                         , tupled $ fmap prettyExpr es
                         , text "of"]
                    <$$> indent 4 (vcat $ fmap prettyCaseAlternative as)
  X.Let a bs e -> vcat [ text "let"
                       , indent 4 $ vcat $ prettyBindList bs
                       , text "in"
                       , indent 4 $ prettyExpr e
                       ]
