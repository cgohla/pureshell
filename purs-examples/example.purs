module Example where

data Perhaps a = Nope | Yeah a

perhaps :: forall a b . b -> (a -> b) -> Perhaps a -> b
perhaps b _ Nope = b
perhaps _ f (Yeah a) = f a