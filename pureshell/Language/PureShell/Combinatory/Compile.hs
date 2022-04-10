{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Language.PureShell.Combinatory.Compile where

import qualified Language.PureShell.Combinatory.Types as Combinatory
import qualified Language.PureShell.Procedural        as Procedural

import           Control.Monad.State                  (MonadState, get, put)
import           Control.Monad.Writer                 (MonadWriter)
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as C8 (pack)
import qualified Data.Map.Strict                      as Map (Map, insertLookupWithKey)

-- lowerToProcedural :: Combinatory.Module -> Procedural.Module ByteString
-- lowerToProcedural m = undefined

lowerTopLevelBind :: (MonadState LocalNames m, MonadWriter [Procedural.FunDef ByteString] m)
                  => Combinatory.TopLevelBind -> m (Procedural.FunDef ByteString)
lowerTopLevelBind (Combinatory.Bind i e) = fmap (Procedural.FunDef fn ps) t -- But for Abs's we need to revover the parameters
  where
    fn = Procedural.FunName $ C8.pack $ show i
    t = lowerExpr e
    ps = case e of
      Combinatory.Abs _ _ -> error "recovering params not implemented"
      _                   -> []

lowerLiteral :: (MonadWriter [Procedural.FunDef ByteString] m)
             => Combinatory.Literal c ->  m (Procedural.Sequence ByteString)
lowerLiteral = \case
  Combinatory.StringLiteral s         -> pure $ literal s
  Combinatory.NumericLiteral (Left n) -> pure $ literal $ show n
  _                                   -> error "not implemented"
  where
    literal = Procedural.Sequence [] . Procedural.Literal . C8.pack

type LocalNames = Map.Map ByteString Integer

insertLookup :: (Ord k) => (v -> v -> v) -> k -> v -> Map.Map k v -> (Maybe v, Map.Map k v)
insertLookup f k v m = Map.insertLookupWithKey (\_ -> \x -> \y -> f x y) k v m

mkVarName :: (MonadState LocalNames m) => ByteString -> m Procedural.VarName
mkVarName b = do
  ns <- get
  let (i, ns') = insertLookup (+) b 1 ns
  let j = maybe mempty show i
  put ns'
  pure $ Procedural.VarName $ b <> (C8.pack j)

lowerExpr :: (MonadState LocalNames m, MonadWriter [Procedural.FunDef ByteString] m)
          => Combinatory.Expr c -> m (Procedural.Sequence ByteString)
lowerExpr = \case
  Combinatory.Var _    -> error "needs vars in procedural"
  Combinatory.Lit l    -> lowerLiteral l
  Combinatory.App e _es -> do
    -- traverse
    let a = Procedural.Application (Procedural.ClosureFromName _n) _vs -- CONTINUE here
    pure $ Procedural.Sequence _as a -- lowerExpr e
    -- recursively build sequences for e and es
  _                    -> error "not implemented"
