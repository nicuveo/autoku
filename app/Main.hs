{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

import           Control.Monad.Extra      (whenJustM)
import           Control.Monad.Loops      (whileJust_)
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.RWS.Strict
import           Data.Has
import qualified Data.HashMap.Strict      as M
import           Data.Maybe
import qualified Data.Vector.Mutable      as V

import           Autoku



-- solver implementation

newtype IOSolver r s a = Solver { runSolver :: RWST r () s IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader r, MonadState s)

instance PrimMonad (IOSolver r s) where
  type PrimState (IOSolver r s) = RealWorld
  primitive = Solver . primitive

instance Has LogLevel r => MonadLogger (IOSolver r s) where
  log = ioLogger

instance ( PrimMonad (IOSolver r s)
         , Has (V.MVector RealWorld Cell) s
         ) => MonadGrid (V.MVector RealWorld Cell) (IOSolver r s) where
  getGrid = retrieve
  setGrid = store



-- main

runWith r s x = fst <$> evalRWST (runSolver x) r s

main :: IO ()
main = do
  grid <- V.replicate 81 empty
  V.write grid (fromEnum e5) [5]
  let constraints = [ kingsRule
                    , knightSumRule 60 e5
                    , knightSumRule 41 e3
                    , knightSumRule 43 e7
                    , knightSumRule 33 c5
                    , knightSumRule 36 g5
                    , knightSumRule 34 e1
                    , knightSumRule 26 e9
                    , knightSumRule 15 a1
                    , knightSumRule 10 a9
                    , knightSumRule 13 i1
                    , knightSumRule 15 i9
                    ]
  res <- runWith (Info, constraints) (grid, [] :: [Point]) $ do
    logInfo "starting"
    flagAllEmpty
    whileJust_ fetch simplify
    export
  putStrLn $ prettyPrint res


{-

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
-}
