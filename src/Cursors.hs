module Cursors where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Sequence as S

import Types
import Points

-- * Creation

-- | Clone the current cursor
clone :: Eval ()
clone = getCursor >>= addCursor

-- | Create a cursor at a specific point in the buffer
newCursor :: (Pos a,Show a) => a -> Eval ()
newCursor p = do
  checkBounds p
  i <- toIdxM p
  x <- getXM p
  addCursor (S.singleton (CursPos Nothing (i,x)))

-- * Removal

-- | Remove the current cursor
remove :: Eval ()
remove = getIdx >>= removeIdx

-- | Remove at a particular index
removeIdx :: CursIdx -> Eval ()
removeIdx i = modify (\s -> s { cursors = S.deleteAt i (cursors s), current = (current s) - 1 })

-- * Modification

-- | Modify the current cursor
update :: (Cursor -> Cursor) -> Eval ()
update f = getIdx >>= updateIdx f

-- | Modify the cursor at a particular index
updateIdx :: (Cursor -> Cursor) -> CursIdx -> Eval ()
updateIdx f i = modify (\s -> s { cursors = S.adjust' f i (cursors s) } )


-- * Switching the current cursor

-- | Switch to the next cursor in the sequence
next :: Eval ()
next = modify (\s -> if (current s) + 1 >= S.length (cursors s) then
                       s
                     else
                       s { current = (current s) + 1 })

-- | Switch to the previous cursor in the sequence
prev :: Eval ()
prev = modify (\s -> if current s == 0 then
                       s
                     else
                       s { current = (current s) - 1 })

-- | Set the current cursor index
setIdx :: CursIdx -> Eval ()
setIdx i = modify (\s -> s { current = i })

-- | Get the current cursor index
getIdx :: Eval CursIdx
getIdx = gets current

-- | Add a cursor to the end of the sequence and set it to the current cursor
addCursor :: Cursor -> Eval ()
addCursor c = modify (\s -> s { cursors = cursors s S.|> (overlap c), current = S.length (cursors s) - 1 })

-- | Get the current cursors
getCursors :: Eval (S.Seq Cursor)
getCursors = gets cursors

-- | Get a cursor by index, failing if the index is out of bounds
getCursorAt :: CursIdx -> Eval Cursor
getCursorAt i = do
  cs <- getCursors
  case cs S.!? i of
    Just c -> return c
    Nothing -> throwError ("No cursor at position " ++ show i)

-- | Get the current cursor
getCursor :: Eval Cursor
getCursor = do
  i <- getIdx
  getCursorAt i

-- | Get selections
getSels :: Cursor -> [(BufIdx,BufIdx)]
getSels = foldMap f
  where
    f c | Nothing <- toRange c = []
        | Just x <- toRange c  = [x]

-- | Get active points
getActives :: Cursor -> [CursPt]
getActives = foldMap (\(CursPos _ x) -> [x])

-- | Get anchor points
getAnchors :: Cursor -> [Maybe CursPt]
getAnchors = foldMap (\(CursPos x _) -> [x])

-- | Get active indices
getActiveIdxs :: Cursor -> [BufIdx]
getActiveIdxs = foldMap (\(CursPos _ (i,_)) -> [i])

-- | Get anchor indices
getAnchorIdxs :: Cursor -> [Maybe BufIdx]
getAnchorIdxs = foldMap f
  where
    f (CursPos Nothing _) = [Nothing]
    f (CursPos (Just (i,_)) _) = [Just i]

-- | Eliminate overlap in a cursor
overlap :: Cursor -> Cursor
overlap cs = combine cs'
  where
    cs' = S.sort cs

    combine :: Cursor -> Cursor
    combine cs | S.EmptyL <- S.viewl cs = cs
               | c S.:< cs' <- S.viewl cs =
                   let (c',cs'') = extend c cs' in c' S.<| combine cs''

    extend :: CursPos -> Cursor -> (CursPos,Cursor)
    extend c@(CursPos Nothing _) cs = (c,cs)
    extend c@(CursPos x _) cs
      | S.EmptyL <- S.viewl cs = (c,cs)
      | (CursPos Nothing (k,_)) S.:< cs' <- S.viewl cs =
          let
            Just (i,j) = toRange c
          in
            if k < j then
              extend c cs'
            else
              (c,cs')
      | c'@(CursPos _ y) S.:< cs' <- S.viewl cs =
          let
            Just (i,j) = toRange c
            Just (k,l) = toRange c'
          in
            if k < j && l < j then
              extend c cs'
            else if k < j then
              extend (CursPos x y) cs'
            else
              (c,cs)
