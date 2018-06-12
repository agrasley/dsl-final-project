module Run where

import Data.Foldable
import qualified Data.Sequence as S
import Control.Monad.State (liftIO,get,runStateT)

import Types
import Pretty

printState :: Eval ()
printState = do
  st <- get
  liftIO $ mapM_ print (lines . toList $ pretty st)

run :: Buffer -> Eval a -> IO (a, State)
run b m = runStateT m (State 0 b (S.singleton (S.singleton (CursPos Nothing (0,0)))))

load :: FilePath -> Eval a -> IO (a, State)
load fp m = do
  s <- readFile fp
  run (S.fromList s) m
