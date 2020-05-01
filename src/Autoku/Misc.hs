module Autoku.Misc where

import           Control.Monad.State
import           Data.Has
import           Data.List


(...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f ... g = \x y -> f $ g x y


retrieve :: (MonadState s m, Has x s) => m x
retrieve = gets getter

store :: (MonadState s m, Has x s) => x -> m ()
store = modify . modifier . const

fastNub :: Ord a => [a] -> [a]
fastNub = map head . group . sort


success :: Applicative f => f Bool
success = pure True
failure :: Applicative f => f Bool
failure = pure False
