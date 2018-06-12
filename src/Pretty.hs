{-# LANGUAGE OverloadedStrings #-}

module Pretty where

import Data.Monoid
import qualified Data.Sequence as S

import Types
import Points
import Actions

pretty :: State -> Text
pretty st = buffer . snd $ foldr prettyHelper (0,st) (cursors st)

prettyHelper :: Cursor -> (Int,State) -> (Int,State)
prettyHelper c (i,st) = (i+1,st')
  where
    istr = S.fromList (show i)
    mkCur str = if i == current st then
        "*" <> str <> "*"
      else
        str
    open = mkCur $ "*[" <> istr <> "*"
    close = mkCur $ "*" <> istr <> "]*"
    single = mkCur $ "*[" <> istr <> "]*"

    st' = let
        (x,y,z) = collect c
      in
        insertPrim z close (insertPrim y single (insertPrim x open st))

    collect :: Cursor -> ([BufIdx],[BufIdx],[BufIdx])
    collect c = foldr collectf ([],[],[]) c

    collectf :: CursPos -> ([BufIdx],[BufIdx],[BufIdx]) -> ([BufIdx],[BufIdx],[BufIdx])
    collectf (CursPos Nothing (idx,_)) (x,y,z) = (x,(idx:y),z)
    collectf c (x,y,z) = let Just (i,j) = toRange c in ((i:x),y,(j:z))
