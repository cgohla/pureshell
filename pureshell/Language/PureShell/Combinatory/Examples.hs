{-# LANGUAGE DataKinds #-}
module Language.PureShell.Combinatory.Examples where

import qualified Language.PureShell.Combinatory.Types as C

import           Data.Singletons

exampleModule2 :: C.Module '[ 'C.Foo1]
exampleModule2 = C.ModuleCons (C.Bind (sing @'C.Foo1) $ c)
                 C.ModuleNil
  where
    c = C.Abs (sing @(C.ConcatContexts (C.SingletonContext 'C.Bar1) (C.SingletonContext 'C.Bar3)))
        (C.App (C.Prim "printf '%s%s' ") ( C.GenExprListCons (C.Var $ sing @'C.Bar1) $
                                           C.GenExprListCons (C.Var $ sing @'C.Bar3) $
                                           C.GenExprListNil
                                         )
        )

bindToFoo1 :: C.Expr C.EmptyContext -> C.Module '[ 'C.Foo1]
bindToFoo1 c = C.ModuleCons (C.Bind (sing @'C.Foo1) $ c) C.ModuleNil

-- TODO examples for top level usage of all Expr constructor

-- Note that we can only bind closed expressions at the top level
exampleLit :: C.Module '[ 'C.Foo1]
exampleLit = bindToFoo1 $ C.Lit $ C.StringLiteral "hello"

exampleApp :: C.Module '[ 'C.Foo1]
exampleApp = bindToFoo1 $ C.App a $ C.genExprListSingle b
  where
    a = C.Prim "echo"
    b = C.Lit $ C.StringLiteral "there"
-- function Foo1 {
--   if test $# -lt 0
--   then
--     echo Foo1 "$@"
--     return 0
--   fi
--   local r="$( echo )" ### WRONG
--   local r1="$( echo there )" ### inefficient
--   "${r:-}" "${r1:-}" ### also inefficient. we should just use the literal values
-- }

exampleAbs ::  C.Module '[ 'C.Foo1]
exampleAbs = bindToFoo1 $ C.Abs p v
  where
    p = sing @(C.SingletonContext 'C.Bar1)
    v = C.Var $ sing @'C.Bar1
-- # output is acceptable
-- function Foo1 {
--   if test $# -lt 1
--   then
--     echo Foo1 "$@"
--     return 0
--   fi
--   local Bar1="$1"
--   echo "${Bar1:-}"
-- }

examplePrim ::  C.Module '[ 'C.Foo1]
examplePrim = bindToFoo1 $ C.Prim "boom"
-- # output is acceptable

-- function Foo1 {
--   if test $# -lt 0
--   then
--     echo Foo1 "$@"
--     return 0
--   fi
--   boom
-- }

exampleLet ::  C.Module '[ 'C.Foo1]
exampleLet = bindToFoo1 $ C.Let b e
  where
    b = C.Bind (sing @'C.Bar3) $ C.Lit $ C.StringLiteral "pow"
    e = C.Var $ sing @'C.Bar3
-- # ouput is acceptable

-- function Foo1 {
--   if test $# -lt 0
--   then
--     echo Foo1 "$@"
--     return 0
--   fi
--   local Bar3="$( echo pow )" ### inefficient
--   echo "${Bar3:-}"
-- }
