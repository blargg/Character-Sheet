module Common.Compose
    ( (<$$>)
    , (<**>)
    )where

import Data.Functor.Compose

infixl 4 <$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fn <$$> x = fmap (fmap fn) x

(<**>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
fn <**> x = getCompose $ Compose fn <*> Compose x
