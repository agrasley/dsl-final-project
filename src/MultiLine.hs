module MultiLine where

import Control.Monad.State
import qualified Data.Sequence as S

import Types
import Cursors
import Points
import Movements

-- | Expand the current cursor at a given point.
expandAt :: (Pos a,Show a) => a -> Eval ()
expandAt p = do
  checkBounds p
  i <- toIdxM p
  x <- getXM p
  update (\c -> overlap $ c S.|> (CursPos Nothing (i,x)))

expandDir :: Mvmt -> Bool
expandDir (Move U _) = True
expandDir (Move L _) = True
expandDir (Search Back _) = True
expandDir (Match Back _) = True
expandDir _ = False

-- | Expand the current cursor by movement. The
--   movement begins at either the upper left or bottom
--   right corner of the cursor, depending on the direction
--   of the movement.
expand :: Mvmt -> Eval Bool
expand m = do
  c <- getCursor
  b <- gets buffer
  let i = if expandDir m then
            getMin c
          else
            getMax c
  x <- getXM i
  let move = movePrim (i,x) m b
  case move of
    Nothing -> return False
    Just (j,_) -> expandAt j >> return True

data CDir = Top | Bottom

top :: CDir
top = Top

bot :: CDir
bot = Bottom

-- | Collapse the current cursor from either the top left
--   or bottom right corner.
collapse :: CDir -> Int -> Eval ()
collapse dir i = update (\c -> S.drop i (sortC c))
  where
    sortC c = case dir of
      Top -> sortTop c
      Bottom -> sortBottom c
