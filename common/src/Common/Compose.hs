module Common.Compose where

infixl 4 <$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fn <$$> x = fmap (fmap fn) x
