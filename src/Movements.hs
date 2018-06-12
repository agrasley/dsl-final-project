module Movements where

import Control.Monad (when)
import Control.Monad.State
import Data.Foldable (toList)
import Data.String (IsString(..))
import qualified Data.Sequence as S
import qualified Data.Array as A
import Text.Regex.TDFA
import Text.Regex.TDFA.TDFA
import Text.Regex.TDFA.ReadRegex

import Types
import Points
import Cursors

data Dir =
    U
  | D
  | L
  | R

data SDir =
    Back
  | Forw

data Mvmt =
    Move Dir Int
  | Search SDir String
  | Match SDir String

-- * Movements

-- | Move the active point of the current cursor.
--   Returns False when all moves fail for the cursor.
move :: Mvmt -> Eval Bool
move m = do
    c <- getCursor
    b <- gets buffer
    let moves = fmap (\x -> movePrim x m b) (getActives c)
    let (c',bool) = applyMoves moves c
    when bool (update (\_ -> overlap c'))
    return bool
  where
    applyMoves ms c = let
        z = zip ms (toList c)
      in
        foldr f (S.empty,False) z
    f (Nothing,c) (cs,b) = (c S.<| cs,b)
    f (Just cp,(CursPos x _)) (cs,b) = ((CursPos x cp) S.<| cs,True)

-- | Move the active point of the current cursor.
--   Returns False and doesn't update the cursor
--   if any moves are unsuccessful.
safeMove :: Mvmt -> Eval Bool
safeMove m = do
    c <- getCursor
    b <- gets buffer
    let moves = fmap (\x -> movePrim x m b) (getActives c)
    case sequence moves of
      Nothing -> return False
      Just moves' -> do
        let c' = applyMoves moves' c
        update (\_ -> overlap c')
        return True
  where
    applyMoves ms c = let
        z = zip ms (toList c)
      in
        foldr f S.empty z
    f (cp,(CursPos x _)) cs = (CursPos x cp) S.<| cs

-- | Move the anchor point of the current cursor.
--   Returns False when all moves fail for the cursor.
moveAnchor :: Mvmt -> Eval Bool
moveAnchor m = do
    c <- getCursor
    b <- gets buffer
    let moves = fmap (\x -> case x of
                        Nothing -> Nothing
                        Just c -> movePrim c m b) (getAnchors c)
    let (c',bool) = applyMoves moves c
    when bool (update (\_ -> overlap c'))
    return bool
  where
    applyMoves ms c = let
        z = zip ms (toList c)
      in
        foldr f (S.empty,False) z
    f (Nothing,c) (cs,b) = (c S.<| cs,b)
    f (Just cp,(CursPos _ x)) (cs,b) = ((CursPos (Just cp) x) S.<| cs,True)

-- | Move the anchor point of the current cursor.
--   Returns False and doesn't update if any moves
--   are unsuccessful or if any parts of the cursor don't
--   have anchors.
safeMoveAnchor :: Mvmt -> Eval Bool
safeMoveAnchor m = do
    c <- getCursor
    b <- gets buffer
    let anchors = sequence (getAnchors c)
    case anchors of
      Nothing -> return False
      Just anchors' -> do
        let moves = fmap (\x -> movePrim x m b) anchors'
        case sequence moves of
          Nothing -> return False
          Just moves' -> do
            let c' = applyMoves moves' c
            update (\_ -> overlap c')
            return True
  where
    applyMoves ms c = let
        z = zip ms (toList c)
      in
        foldr f S.empty z
    f (cp,(CursPos _ x)) cs = (CursPos (Just cp) x) S.<| cs


{-
shift :: Mvmt -> Eval Bool
shift m = do
  c <- getCursor
-}

-- * Smart constructors

-- | Move right smart constructor
right :: Int -> Mvmt
right i | i < 0 = Move L (abs i)
        | otherwise = Move R i

-- | Move left smart constructor
left :: Int -> Mvmt
left i | i < 0 = Move R (abs i)
       | otherwise = Move L i

-- | Move up smart constructor
up :: Int -> Mvmt
up i | i < 0 = Move D (abs i)
     | otherwise = Move U i

-- | Move down smart constructor
down :: Int -> Mvmt
down i | i < 0 = Move U (abs i)
       | otherwise = Move D i

-- | Backwards smart constructor
back :: SDir
back = Back

-- | Forwards smart constructor
forw :: SDir
forw = Forw

-- | Search smart constructor
search :: SDir -> String -> Mvmt
search = Search

-- | Match smart constructor
match :: SDir -> String -> Mvmt
match = Match


movePrim :: CursPt -> Mvmt -> Buffer -> Maybe (CursPt)
movePrim (idx,maxX) (Move U i) b | y == 0    = Nothing
                                 | y - i < 0 = Just (toIdx ((maxX,0)::Point) b,maxX)
                                 | otherwise = Just (toIdx (maxX,y-i) b,maxX)
                                   where
                                     (x,y) = toPt idx b
movePrim (idx,maxX) (Move D i) b | idx' == idx = Nothing
                                 | otherwise   = Just (idx',maxX)
                                   where
                                     (x,y) = toPt idx b
                                     idx'  = toIdx (maxX,y+i) b
movePrim (idx,maxX) (Move L i) b | x == 0    = Nothing
                                 | x - i < 0 = Just (toIdx ((0,y)::Point) b,0)
                                 | otherwise = Just (toIdx (x-i,y) b,x-i)
                                   where
                                     (x,y) = toPt idx b
movePrim (idx,maxX) (Move R i) b | idx' == idx = Nothing
                                 | otherwise   = Just (idx',x')
                                   where
                                     (x,y) = toPt idx b
                                     idx'  = toIdx (x+i,y) b
                                     (x',_) = toPt idx' b
movePrim (idx,_) (Search Forw re) b =
  case matches of
    [] -> Nothing
    xs -> Just (idx',getX idx' b)
  where
    idx' = idx+getOffset matches

    matches :: [(MatchOffset, MatchLength)]
    matches = getAllSubmatches $ S.drop idx b =~ re

    getOffset ((o,_):xs) = go o xs

    go o ((o',0):xs) = go o' xs
    go o (x:xs) = go o xs
    go o [] = o
movePrim (idx,_) (Search Back re) b =
  case matches of
    [] -> Nothing
    xs -> Just (idx',getX idx' b)
  where
    idx' = getOffset matches

    matches :: [(MatchOffset, MatchLength)]
    matches = A.elems $ last ((S.take idx b =~ re) :: [MatchArray])

    getOffset ((o,_):xs) = go o xs

    go o ((o',0):xs) = go o' xs
    go o (x:xs) = go o xs
    go o [] = o
movePrim (idx,_) (Match Forw re) b =
  case matches of
    [] -> Nothing
    ((0,_):_) -> Just (idx',getX idx' b)
    xs -> Nothing
  where
    idx' = idx+getOffset matches

    matches :: [(MatchOffset, MatchLength)]
    matches = getAllSubmatches $ S.drop idx b =~ re

    getOffset ((o,l):xs) = go l xs

    go o ((o',0):xs) = go o' xs
    go o (x:xs) = go o xs
    go o [] = o
movePrim (idx,_) (Match Back re) b =
  case matches of
    [] -> Nothing
    ((0,_):_) -> Just (idx',getX idx' b)
    xs -> Nothing
  where
    idx' = idx-getOffset matches

    matches :: [(MatchOffset, MatchLength)]
    matches = getAllSubmatches $ S.reverse (S.take idx b) =~ re

    getOffset ((o,l):xs) = go l xs

    go o ((o',0):xs) = go o' xs
    go o (x:xs) = go o xs
    go o [] = o
