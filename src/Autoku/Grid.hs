{-# LANGUAGE FunctionalDependencies #-}

module Autoku.Grid where


import           Control.Monad.Extra
import           Control.Monad.Primitive
import qualified Data.HashMap.Strict     as M
import           Data.Maybe
import qualified Data.Vector.Mutable     as V

import           Autoku.Cell
import           Autoku.Point



-- abstract grid

class Monad m => Grid g m where
  gRead   :: g -> Point -> m Cell
  gWrite  :: Point -> Cell -> g -> m (Maybe g)
  gModify :: (Cell -> Cell) -> Point -> g -> m (Maybe g)
  gExport :: g -> m [Cell]

class Grid g m => MonadGrid g m | m -> g where
  getGrid :: m g
  setGrid :: g -> m ()



-- practical use

readM :: MonadGrid g m => Point -> m Cell
readM p = do
  g <- getGrid
  gRead g p

writeM :: MonadGrid g m => Point -> Cell -> m ()
writeM p c = do
  g <- getGrid
  whenJustM (gWrite p c g) setGrid

modifyM :: MonadGrid g m => (Cell -> Cell) -> Point -> m ()
modifyM f p = do
  g <- getGrid
  whenJustM (gModify f p g) setGrid

export :: MonadGrid g m => m [Cell]
export = gExport =<< getGrid



-- pure grid

instance Monad m => Grid (M.HashMap Point Cell) m where
  gRead   g p   = pure $ fromMaybe empty $ M.lookup p g
  gWrite  p x g = pure $ Just $ M.insert p x g
  gModify f p g = pure $ Just $ M.adjust f p g
  gExport g     = pure [fromMaybe empty $ M.lookup p g | p <- allPoints]



-- mutable grid

instance (PrimMonad m, s ~ PrimState m) => Grid (V.MVector s Cell) m where
  gRead   g p   = V.unsafeRead   g (fromEnum p)
  gWrite  p x g = V.unsafeWrite  g (fromEnum p) x >> pure Nothing
  gModify f p g = V.unsafeModify g f (fromEnum p) >> pure Nothing
