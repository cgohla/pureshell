{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Language.PureShell.Combinatory.Examples where

import qualified Language.PureShell.Combinatory.IR as C

import           Data.Singletons
import           GHC.TypeLits

exampleModule2 :: C.Module Symbol '[ "foo"]
exampleModule2 = C.ModuleCons (C.Bind (sing @"foo") $ c)
                 C.ModuleNil
  where
    c = C.Abs (sing @(C.ConcatContexts (C.SingletonContext "bar") (C.SingletonContext "baz")))
        (C.App (C.Prim "printf") ( C.GenExprListCons (C.Lit $ C.StringLiteral "%s%s") $
                                   C.GenExprListCons (C.Var $ sing @"bar") $
                                   C.GenExprListCons (C.Var $ sing @"baz") $
                                   C.GenExprListNil
                                 )
        )

bindToFoo :: C.Expr Symbol C.EmptyContext -> C.Module Symbol '[ "foo"]
bindToFoo c = C.ModuleCons (C.Bind (sing @"foo") $ c) C.ModuleNil

-- | This is just debugging crutch for now
getHeadExpr :: C.Module ids ((a :: ids) ': as) -> C.Expr ids C.EmptyContext
getHeadExpr (C.ModuleCons (C.Bind _ e) _) = e
-- surprise, C.lowerModule already does this

-- TODO examples for top level usage of all Expr constructors.
-- TODO make examples with deeper structure.

-- Note that we can only bind closed expressions at the top level
exampleLit :: C.Module Symbol '[ "foo"]
exampleLit = bindToFoo $ C.Lit $ C.StringLiteral "hello"
-- # good
-- #!/bin/bash
-- set -o errexit -o nounset -o pipefail

-- function Foo1 {
--   echo hello
-- }

exampleApp :: C.Module Symbol '[ "foo"]
exampleApp = bindToFoo $ C.App a $ C.genExprListSingle b
  where
    a = C.Prim "hello"
    b = C.Lit $ C.StringLiteral "there"
-- ## good.

-- #!/bin/bash
-- set -o errexit -o nounset -o pipefail

-- function Foo1 {
--   local r=hello
--   local r1=there
--   "${r:-}" "${r1:-}" ### we may want to eliminate the indirection as an optional optimization
-- }

exampleAbs ::  C.Module Symbol '[ "foo"]
exampleAbs = bindToFoo $ C.Abs p v
  where
    p = sing @(C.SingletonContext "bar")
    v = C.Var $ sing @"bar"
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

examplePrim ::  C.Module Symbol '[ "foo"]
examplePrim = bindToFoo $ C.Prim "boom"
-- # good

-- #!/bin/bash
-- set -o errexit -o nounset -o pipefail

-- function Foo1 {
--   echo boom
-- }

exampleLet ::  C.Module Symbol '[ "foo"]
exampleLet = bindToFoo $ C.Let b e
  where
    b = C.Bind (sing @"baz") $ C.Lit $ C.StringLiteral "pow"
    e = C.Var $ sing @"baz"
-- # ouput is acceptable

-- #!/bin/bash
-- set -o errexit -o nounset -o pipefail

-- function Foo1 {
--   local Bar3=pow ## TODO we might still want to optimize away the indirection
--   echo "${Bar3:-}"
-- }
