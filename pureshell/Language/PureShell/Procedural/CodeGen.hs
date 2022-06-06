module Language.PureShell.Procedural.CodeGen where

import qualified Data.ByteString.ShellEscape      as Escape (bash)
import qualified Language.Bash                    as Bash
import qualified Language.Bash.Syntax             as Bash (Identifier (..),
                                                           SpecialVar (..),
                                                           Trim (..))

import qualified Language.PureShell.Identifiers   as Ids
import qualified Language.PureShell.Procedural.IR as P

appendBashStatements :: [Bash.Statement ()] -> Bash.Statement ()
appendBashStatements [] = Bash.Empty
appendBashStatements (s:ss) = Bash.Sequence (ann s) $ ann $ appendBashStatements ss
  where
    ann = Bash.Annotated ()

applicationAsStatement :: P.FunClosure -> [Ids.LocalBashVarName] -> Bash.Statement ()
applicationAsStatement f vs = Bash.SimpleCommand f' vs'
  where
    vs' = fmap (readFromVar . Ids.getVarName) vs
    f' = case f of
           P.ClosureFromName n -> Bash.Literal $ Escape.bash $ Ids.getFunName n
           P.ClosureFromVar v  -> readFromVar $ Ids.getVarName v
    readFromVar = Bash.ReadVarSafe . Bash.VarIdent . Bash.Identifier
