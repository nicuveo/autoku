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
      logDebug "backlog: empty"
      pure Nothing
    (x:xs) -> do
      store xs
      pure $ Just x

flagAll :: (MonadGrid g m, MonadLogger m, MonadBacklog s m) => m ()
flagAll =
  for_ allPoints $ \p -> unlessM (isSolved <$> readM p) $ flag p



-- solving

type MonadSolver g r s b m =
  ( MonadGrid        g m
  , MonadBacklog     s m
  , MonadLogger        m
  , MonadConstraints r m
  )

simplify :: MonadSolver g r s b m => Point -> m Bool
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
  if null newSet
  then do
    logDebug $ printf "simplify: inconsistency found for %s!" (show p)
    pure False
  else do
    when (newSet /= current) $ do
      writeM p newSet
      logDebug $ case value newSet of
        Nothing -> printf "simplify: %s cannot be %s => %s" (show p) (show $ current \\ newSet) (show newSet)
        Just x  -> printf "simplify: %s is solved: %d" (show p) x
      traverse_ flag $ concat points ++ related
    pure True


simplifyAll :: MonadSolver g r s b m => m Bool
simplifyAll = do
  nextPoint <- fetch
  case nextPoint of
    Nothing -> success
    Just p  -> simplify p &&^ simplifyAll

isFinished :: MonadSolver g r s b m => m Bool
isFinished = allM (fmap isSolved . readM) allPoints

findNextGuess :: MonadSolver g r s b m => m (Point, Int)
findNextGuess = do
  candidates <- for allPoints $ \p -> do
    cell <- readM p
    if isSolved cell then pure Nothing else do
      constraints <- filter (applies p) <$> asks getter
      pure $ Just ( (length $ possible cell, negate $ length constraints)
                  , (p, head cell)
                  )
  pure $ snd $ minimum $ catMaybes candidates

solve :: MonadSolver g r s b m => m Bool
solve = flagAll >> simplifyAll &&^ (isFinished ||^ guess)
  where
    guess = do
      (point, guessed) <- findNextGuess
      guesses <- length <$> getStack
      logInfo $ printf "solve: guessing that %s is %d (guesses: %d)"
        (show point) guessed guesses
      void pushGrid
      writeM point $ solved guessed
      (||^) solve $ do
        void popGrid
        logInfo $ printf "solve: rolling back: %s can't be %d" (show point) guessed
        modifyM (restrict guessed) point
        solve
