{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Bash.Test where

import qualified Language.Bash    as Bash (Expression (..), Statement (..))
import qualified Text.ShellEscape as Escape (bash)

data Test t where
  -- | File test
  Test_a :: Bash.Expression t -> Test t -- ^ -a
  Test_b :: Bash.Expression t -> Test t
  Test_c :: Bash.Expression t -> Test t
  Test_d :: Bash.Expression t -> Test t
  Test_e :: Bash.Expression t -> Test t
  Test_f :: Bash.Expression t -> Test t
  Test_g :: Bash.Expression t -> Test t
  Test_h :: Bash.Expression t -> Test t
  Test_L :: Bash.Expression t -> Test t
  Test_k :: Bash.Expression t -> Test t
  Test_p :: Bash.Expression t -> Test t
  Test_r :: Bash.Expression t -> Test t
  Test_s :: Bash.Expression t -> Test t
  Test_S :: Bash.Expression t -> Test t
  Test_t :: Bash.Expression t -> Test t
  Test_u :: Bash.Expression t -> Test t
  Test_w :: Bash.Expression t -> Test t
  Test_x :: Bash.Expression t -> Test t
  Test_O :: Bash.Expression t -> Test t
  Test_G :: Bash.Expression t -> Test t
  Test_N :: Bash.Expression t -> Test t
  Test_nt  :: Bash.Expression t -> Bash.Expression t -> Test t
  Test_ot  :: Bash.Expression t -> Bash.Expression t -> Test t
  Test_ef  :: Bash.Expression t -> Bash.Expression t -> Test t
  -- | String operators
  Test_z :: Bash.Expression t -> Test t
  Test_n :: Bash.Expression t -> Test t
  Test_equal  :: Bash.Expression t -> Bash.Expression t -> Test t -- ^ =
  Test_unequal  :: Bash.Expression t -> Bash.Expression t -> Test t -- ^ !=
  Test_lexLT  :: Bash.Expression t -> Bash.Expression t -> Test t -- ^ <
  Test_lexGT  :: Bash.Expression t -> Bash.Expression t -> Test t -- ^ >
  -- | Other operators
  Test_o :: Bash.Expression t -> Test t
  Test_v :: Bash.Expression t -> Test t
  Test_R :: Bash.Expression t -> Test t
  Test_negate :: Test t -> Test t -- ^ !
  Test_and ::  Test t -> Test t -> Test t
  Test_or ::  Test t -> Test t -> Test t
  -- | Arithmetic test
  Test_eq :: Bash.Expression t -> Bash.Expression t -> Test t
  Test_ne :: Bash.Expression t -> Bash.Expression t -> Test t
  Test_lt :: Bash.Expression t -> Bash.Expression t -> Test t
  Test_le :: Bash.Expression t -> Bash.Expression t -> Test t
  Test_gt :: Bash.Expression t -> Bash.Expression t -> Test t
  Test_ge :: Bash.Expression t -> Bash.Expression t -> Test t

test :: Test t -> Bash.Statement t
test  =  Bash.SimpleCommand (lit "test") . go
  where
    go = \case
      Test_a e         -> go1 "a" e
      Test_b e         -> go1 "b" e
      Test_c e         -> go1 "c" e
      Test_d e         -> go1 "d" e
      Test_e e         -> go1 "e" e
      Test_f e         -> go1 "f" e
      Test_g e         -> go1 "g" e
      Test_h e         -> go1 "h" e
      Test_L e         -> go1 "L" e
      Test_k e         -> go1 "k" e
      Test_p e         -> go1 "p" e
      Test_r e         -> go1 "r" e
      Test_s e         -> go1 "s" e
      Test_S e         -> go1 "S" e
      Test_t e         -> go1 "t" e
      Test_u e         -> go1 "u" e
      Test_w e         -> go1 "w" e
      Test_x e         -> go1 "x" e
      Test_O e         -> go1 "O" e
      Test_G e         -> go1 "G" e
      Test_N e         -> go1 "N" e
      Test_nt e f      -> go2 "nt" e f
      Test_ot e f      -> go2 "ot" e f
      Test_ef e f      -> go2 "ef" e f
      Test_z e         -> go1 "z" e
      Test_n e         -> go1 "n" e
      Test_equal e f   -> go2' "=" e f -- TODO make sure it's not escaped
      Test_unequal e f -> go2' "!=" e f -- TODO make sure it's not escaped
      Test_lexLT e f   -> go2' "<" e f -- TODO make sure it's not escaped
      Test_lexGT e f   -> go2' ">" e f -- TODO make sure it's not escaped
      Test_o e         -> go1 "o" e
      Test_v e         -> go1 "v" e
      Test_R e         -> go1 "R" e
      Test_negate t    -> [lit "!"] <> go t -- TODO make sure it's not escaped
      Test_and t t'    -> conn "a" t t'
      Test_or t t'     -> conn "o" t t'
      Test_eq e e'     -> go2 "eq" e e'
      Test_ne e e'     -> go2 "ne" e e'
      Test_lt e e'     -> go2 "lt" e e'
      Test_le e e'     -> go2 "le" e e'
      Test_gt e e'     -> go2 "gt" e e'
      Test_ge e e'     -> go2 "ge" e e'
    go1 x e = [lit $ "-" <> x, e]
    go2 x e f = [e, lit $ "-" <> x, f]
    go2' x e f = [e, lit x, f]
    lit = Bash.Literal . Escape.bash
    parens es = [lit "("] <> es <> [lit ")"]
    conn x t t' =  (parens $ go t) <> [lit $ "-" <> x] <> (parens $ go t')
