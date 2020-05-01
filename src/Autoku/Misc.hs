module Autoku.Misc where

import           Control.Monad.State
import           Data.Has


(...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f ... g = \x y -> f $ g x y


store :: (MonadState s m, Has x s) => x -> m ()
store = modify . modifier . const
