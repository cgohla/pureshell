module TopRecDecl where

data Nat = Z | S Nat

add ∷ Nat → Nat → Nat
add Z n = n
add (S m) n = add m (S n)

gauss ∷ Nat → Nat
gauss = go Z
  where
    go a Z = a
    go a (S n) = go (add a (S n)) n
