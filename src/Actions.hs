module Actions where

import Control.Monad.State (modify)
import Data.List (sortBy)
import qualified Data.Sequence as S

import Points
import Types
import Cursors

-- * Selection

select :: Eval ()
select = update (fmap f)
  where
    f (CursPos Nothing x) = CursPos (Just x) x
    f x = x

unselect :: Eval ()
unselect = update (fmap (\(CursPos _ x) -> CursPos Nothing x))

deleteSel :: Eval ()
deleteSel = do
  c <- getCursor
  modify (deletePrim (getSels c))
  unselect

replaceSel :: Text -> Eval ()
replaceSel t = deleteSel >> insert t

-- * Insertion

insert :: Text -> Eval ()
insert t = do
  c <- getCursor
  modify (insertPrim (getActiveIdxs c) t)


{-
Basic actions
-}

adjustCursPos :: Pos a => a -> Int -> Buffer -> CursPos -> CursPos
adjustCursPos p len b (CursPos Nothing (i,maxX))
  | toIdx p b < i = let j = i+len in CursPos Nothing (j,getX j b)
  | otherwise     = CursPos Nothing (i,getX i b)
adjustCursPos p len b (CursPos (Just (i,maxX)) (j,maxX'))
  | k < i && k < j = CursPos (Just ip') jp'
  | k < i          = CursPos (Just ip') jp
  | k < j          = CursPos (Just ip) jp'
  | otherwise      = CursPos (Just ip) jp
    where
      i' = i+len
      ip' = (i',getX i' b)
      ip = (i,getX i b)
      j' = j+len
      jp = (j,getX j b)
      jp' = (j',getX j' b)
      k = toIdx p b

adjustAfter :: Pos a => a -> Int -> State -> State
adjustAfter p len (State c b cs) = State c b (fmap (fmap (adjustCursPos p len b)) cs)

insertPrim :: Pos a => [a] -> Text -> State -> State
insertPrim [] _ st = st
insertPrim (p:ps) t (State c b cs) = st'
  where
    (l,r) = S.splitAt (toIdx p b) b
    st = State c (l S.>< t S.>< r) cs
    st' = adjustAfter p (length t) st

delAdjustCursPos :: (BufIdx,BufIdx) -> Buffer -> CursPos -> CursPos
delAdjustCursPos r b (CursPos s e) = CursPos (s' s) (f r (fst e))
  where
    s' Nothing = Nothing
    s' (Just e') = Just (f r (fst e'))

    f :: (BufIdx,BufIdx) -> BufIdx -> (BufIdx,MaxX)
    f (i,j) k | k < j, k > i = (i,getX i b)
              | k >= j = let l = k - j - i in (l,getX l b)
              | otherwise = (k,getX k b)

delAdjust :: (BufIdx,BufIdx) -> Buffer -> S.Seq Cursor -> S.Seq Cursor
delAdjust r b cs = fmap (fmap (delAdjustCursPos r b)) cs

deletePrim :: Pos a => [(a,a)] -> State -> State
deletePrim ps (State c b cs) = State c b' (fmap overlap cs')
  where
    b' = (go (combine sortStart) b)

    ps' = combine sortStart

    cs' = g ps' cs

    g [] cs = cs
    g (p:ps) cs = g ps (delAdjust p b' cs)

    go [] b = b
    go ((i,j):is) b = let
        b'    = go is b
        (l,r) = S.splitAt i b'
      in l S.>< (S.drop (j-i) r)

    is = fmap (\(i,j) -> (toIdx i b, toIdx j b)) ps
    ord (s,e) (s',e') | s < s' = LT
                      | s > s' = GT
                      | e > e' = LT
                      | e < e' = GT
                      | otherwise = EQ
    sortStart = sortBy ord is

    combine :: [(BufIdx,BufIdx)] -> [(BufIdx,BufIdx)]
    combine [] = []
    combine (i:is) = let (i',is') = extend i is in i' : combine is'

    extend :: (BufIdx,BufIdx) -> [(BufIdx,BufIdx)] -> ((BufIdx,BufIdx),[(BufIdx,BufIdx)])
    extend x [] = (x, [])
    extend (i,j) x@((k,l):is) | k < j && j < l = extend (i,l) is
                              | k < j          = extend (i,j) is
                              | otherwise      = ((i,j), x)
