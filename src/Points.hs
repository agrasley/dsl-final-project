{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Points where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Sequence as S

import Types

checkBounds :: (Pos a, Show a) => a -> Eval ()
checkBounds p = do
  b <- gets buffer
  i <- toIdxM p
  if i > S.length b then
    throwError ("Position " ++ show p ++ " out of bounds")
  else
    return ()

getX :: Pos a => a -> Buffer -> Int
getX p b = fst $ toPt p b

getXM :: Pos a => a -> Eval Int
getXM p = toPtM p >>= return . fst

getY :: Pos a => a -> Buffer -> Int
getY p b = snd $ toPt p b

getYM :: Pos a => a -> Eval Int
getYM p = toPtM p >>= return . snd

-- | Convert an index of the buffer to a 2D point
idxToPt :: BufIdx -> Buffer -> Point
idxToPt idx buf = (x,y)
  where
    x = length $ S.drop (head idxs + 1) before
    y = length idxs
    idxs = S.elemIndicesR '\n' before
    before = S.take idx buf

-- | Convert a 2D point to an index of the buffer
ptToIdx :: Point -> Buffer -> BufIdx
ptToIdx (x,y) buf | x < end - start = start + x
                  | otherwise       = end
  where
    len = S.length buf
    lidxs = length idxs
    start = if y >= lidxs then
        last idxs
      else
        (idxs !! y-1) + 1
    end = if y >= lidxs then
        len
      else
        idxs !! y
    idxs = S.elemIndicesL '\n' buf

class Pos a where
  toIdx :: a -> Buffer -> BufIdx
  toPt  :: a -> Buffer -> Point
  toIdxM :: a -> Eval BufIdx
  toIdxM a = do
    b <- gets buffer
    return (toIdx a b)
  toPtM :: a -> Eval Point
  toPtM a = do
    b <- gets buffer
    return (toPt a b)

instance Pos Point where
  toIdx = ptToIdx
  toPt  = const

instance Pos BufIdx where
  toIdx = const
  toPt  = idxToPt
