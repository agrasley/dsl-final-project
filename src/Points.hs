{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Points where

import Debug.Trace
import Control.Monad.State
import qualified Data.Sequence as S

import Types

checkBounds :: (Pos a, Show a) => a -> Eval ()
checkBounds p = do
  b <- gets buffer
  i <- toIdxM p
  if i > S.length b then
    error ("Position " ++ show p ++ " out of bounds")
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
    x = case newlines of
      [] -> idx
      _  -> idx - (1 + head newlines)

    y :: Int
    y = length newlines

    newlines :: [BufIdx]
    newlines = S.elemIndicesR '\n' textBefore

    textBefore :: Text
    textBefore = S.take idx buf

-- | Convert a 2D point to an index of the buffer
ptToIdx :: Point -> Buffer -> BufIdx
ptToIdx (x,y) buf | x > end - start = trace ("end: " ++ show end ++ " start: " ++ show start ++ " x: " ++ show x) $ end
                  | otherwise       = trace ("end: " ++ show end ++ " start: " ++ show start ++ " x: " ++ show x) $ start + x
  where
    start :: BufIdx
    start = if y == 0 then
        0
      else if y >= length newlines then
        last newlines + 1
      else
        (newlines !! (y-1)) + 1

    end :: BufIdx
    end = if y >= length newlines then
        S.length buf
      else
        newlines !! y

    newlines :: [BufIdx]
    newlines = S.elemIndicesL '\n' buf

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
