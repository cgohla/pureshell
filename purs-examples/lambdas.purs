module Example.Lambdas where

const :: forall a b. a -> b -> a
const a _ = a

flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f a b = f b a

-- | Church encoding of pairs
pair :: forall a b c. a -> b -> (a -> b -> c) -> c
pair a b f = f a b

fst :: forall a b . ((a -> b -> a) -> a) -> a
fst p = p const

snd :: forall a b. ((a -> b -> b) -> b) -> b
snd p = p (flip const)
