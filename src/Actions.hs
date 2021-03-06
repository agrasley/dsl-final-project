module Actions where

import Control.Monad.State (gets,modify)
import Data.List (sort,sortBy,intersperse)
import Data.Maybe
import qualified Data.Sequence as S

import Points
import Types
import Cursors
import Movements

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

-- * Deletion

delete :: Mvmt -> Eval Bool
delete m = do
    c <- getCursor
    b <- gets buffer
    let moves = fmap (\x -> fmap fst (movePrim x m b)) (getActives c)
    let z = catMaybes $ zipWith f moves (getActiveIdxs c)
    case z of
      [] -> return False
      _  -> do
        modify (deletePrim z)
        return True
  where
    f Nothing _ = Nothing
    f (Just i) j | i < j = Just (i,j)
                 | otherwise = Just (j,i)




{-
Basic actions
-}

adjustCursPos :: BufIdx -> Int -> Buffer -> CursPos -> CursPos
adjustCursPos k len b (CursPos Nothing (i,maxX))
  | k <= i    = let j = i+len in CursPos Nothing (j,getX j b)
  | otherwise = CursPos Nothing (i,getX i b)
adjustCursPos k len b (CursPos (Just (i,maxX)) (j,maxX'))
  | k <= i && k <= j = CursPos (Just ip') jp'
  | k <= i           = CursPos (Just ip') jp
  | k <= j           = CursPos (Just ip) jp'
  | otherwise        = CursPos (Just ip) jp
    where
      i' = i+len
      ip' = (i',getX i' b)
      ip = (i,getX i b)
      j' = j+len
      jp = (j,getX j b)
      jp' = (j',getX j' b)

adjustAfter :: BufIdx -> Int -> Buffer -> S.Seq Cursor -> S.Seq Cursor
adjustAfter i len b cs = fmap (fmap (adjustCursPos i len b)) cs

insertBuf :: Pos a => [a] -> Text -> Buffer -> Buffer
insertBuf ps t b = mconcat $ intersperse t bs
  where
    is :: [BufIdx]
    is = sort (fmap (\p -> toIdx p b) ps)

    bs :: [Text]
    bs = foldr (\i bs -> let
                      (x,y) = S.splitAt i (head bs)
                    in
                      (x:y:tail bs)
               ) [b] is


insertPrim :: Pos a => [a] -> Text -> State -> State
insertPrim [] _ st = st
insertPrim ps t (State c b cs) = State c b' cs'
  where
    b' = insertBuf ps t b
    cs' = foldr (\p cs -> adjustAfter (toIdx p b) (S.length t) b cs) cs ps


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
