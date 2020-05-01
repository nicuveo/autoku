{-# LANGUAGE ScopedTypeVariables #-}

module Autoku.Solver where


import           Control.Monad.Extra
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Foldable
import           Data.Has
import           Data.List
import           Data.Maybe
import           Data.String
import           Data.Traversable
import           Text.Printf

import           Autoku.Cell
import           Autoku.Constraint
import           Autoku.Grid
import           Autoku.Logging
import           Autoku.Misc
import           Autoku.Point



-- monadic stuff

type MonadBacklog     s m = (MonadState  s m, Has [Point] s)
type MonadConstraints r m = (MonadReader r m, Has [Constraint] r)



-- backlog

flag :: (MonadGrid g m, MonadLogger m, MonadBacklog s m) => Point -> m ()
flag p =
  unlessM (isSolved <$> readM p) $ do
    backlog <- retrieve
    unless (p `elem` backlog) $ do
      logDebug $ "backlog: flagging " ++ show p
      store $ p:backlog

fetch :: (MonadLogger m, MonadBacklog s m) => m (Maybe Point)
fetch = do
  bl <- retrieve
  case bl of
    [] -> do
      logInfo "backlog: empty"
      pure Nothing
    (x:xs) -> do
      store xs
      pure $ Just x



-- simplify

type MonadSolver g r s b m =
  ( MonadGrid        g m
  , MonadBacklog     s m
  , MonadLogger        m
  , MonadConstraints r m
  )

simplify :: MonadSolver g r s b m => Point -> m ()
simplify p = do
  current <- readM p
  when (isSolved current) $
    logFatal $ fromString $ "/!\\ tried to simplify solved point " ++ show p
  constraints  <- filter (applies p) <$> asks getter
  (points, rf) <- fmap unzip $ for constraints $ \c -> do
    let ps = cOthers c p
    cells <- traverse readM ps
    pure (ps, cRestrict c cells)
  let related = concat [row p, column p, square p]
  rCells <- mapMaybe value <$> traverse readM related
  let newSet = foldr ($) current $ rf ++ map restrict rCells
  when (newSet /= current) $ do
    writeM p newSet
    logInfo $ case value newSet of
      Nothing -> printf "simplify: %s cannot be %s => %s" (show p) (show $ current \\ newSet) (show newSet)
      Just x  -> printf "simplify: %s is solved: %d" (show p) x
    traverse_ flag $ concat points ++ related
