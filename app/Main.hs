{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

import           Control.Monad.Extra      (whenJustM)
import           Control.Monad.Loops      (whileJust_)
import           Control.Monad.Reader
import           Control.Monad.RWS.Strict
import           Data.Has
import qualified Data.HashMap.Strict      as M
import           Data.Maybe

import           Autoku



-- solver implementation

newtype IOSolver r s a = Solver { runSolver :: RWST r () s IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader r, MonadState s)

instance Has LogLevel r => MonadLogger (IOSolver r s) where
  log = ioLogger

type GridImplem = M.HashMap Point Cell

instance Has GridImplem s => MonadGrid GridImplem (IOSolver r s) where
  getGrid = retrieve
  setGrid = store



-- main

runWith r s x = fst <$> evalRWST (runSolver x) r s

main :: IO ()
main = do
  let grid        = test
      backlog     = [p | p <- allPoints, empty == grid M.! p]
      constraints = [] :: [Constraint]
  putStrLn $ prettyPrint i
  res <- runWith (Info, constraints) (grid, backlog) $ do
    logInfo "starting"
    whileJust_ fetch simplify
    export
  putStrLn $ prettyPrint res


test :: GridImplem
test = M.fromList $ zip allPoints i

i = map f
  [e,e,3 ,e,e,2 ,1,5,4
  ,e,4,e ,e,3,e ,9,e,e
  ,e,6,e ,1,5,e ,8,e,e

  ,e,2,7 ,4,e,e ,e,e,e
  ,6,e,e ,e,e,e ,e,9,e
  ,e,9,5 ,e,7,e ,e,3,8

  ,2,e,9 ,6,e,1 ,e,e,e
  ,4,7,e ,e,8,e ,e,e,5
  ,e,e,e ,e,e,7 ,6,e,9
  ]
  where e = 0
        f 0 = empty
        f x = [x]
