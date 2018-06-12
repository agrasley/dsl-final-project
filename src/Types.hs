module Types where

import qualified Control.Monad.State as St
import qualified Data.Sequence as S

type Text   = S.Seq Char
type Buffer = Text
type BufIdx = Int
type CursIdx = Int
type MaxX   = Int
type Point  = (Int,Int)
type CursPt = (BufIdx,MaxX)
type Eval   = St.StateT State IO

data CursPos = CursPos { anchor :: Maybe CursPt, active :: CursPt }
  deriving (Eq,Show)

toRange :: CursPos -> Maybe (BufIdx,BufIdx)
toRange (CursPos Nothing _) = Nothing
toRange (CursPos (Just (i,_)) (j,_)) | i <= j = Just (i,j)
                                     | otherwise = Just (j,i)

instance Ord CursPos where
  compare (CursPos Nothing (i,_)) (CursPos Nothing (j,_))
    | i < j  = LT
    | i > j  = GT
    | i == j = EQ
  compare (CursPos Nothing (i,_)) x =
    let Just (j,_) = toRange x in if i < j then LT else GT
  compare x (CursPos Nothing (j,_)) =
    let Just (i,_) = toRange x in if j < i then GT else LT
  compare x y =
    let
      Just (i,j) = toRange x
      Just (k,l) = toRange y
    in
      if i < k then
        LT
      else if i > k then
        GT
      else if l > j then
        GT
      else if l < j then
        LT
      else
        EQ

type Cursor = S.Seq CursPos

data State = State {
    current :: CursIdx,
    buffer  :: Buffer,
    cursors :: S.Seq Cursor
  } deriving (Eq,Show)
