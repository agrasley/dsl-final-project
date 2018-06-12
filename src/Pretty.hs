{-# LANGUAGE OverloadedStrings #-}

module Pretty where

import Data.Monoid
import qualified Data.Sequence as S

import Types
import Points
import Actions

pretty :: State -> Text
pretty st = buffer $ foldr prettyHelper st [0..(S.length (cursors st) - 1)]

prettyOpen :: Cursor -> Int -> State -> State
prettyOpen c i st = insertPrim (collect c) open st
  where
    mkCur :: Text -> Text
    mkCur str = if i == current st then
        "*" <> str <> "*"
      else
        str
    istr = S.fromList (show i)
    open = mkCur $ "*[" <> istr <> "*"

    collect :: Cursor -> [BufIdx]
    collect c = foldr collectf [] c

    collectf :: CursPos -> [BufIdx] -> [BufIdx]
    collectf c xs = case toRange c of
      Nothing -> xs
      Just (i,_) -> i:xs

prettyClose :: Cursor -> Int -> State -> State
prettyClose c i st = insertPrim (collect c) close st
  where
    mkCur :: Text -> Text
    mkCur str = if i == current st then
        "*" <> str <> "*"
      else
        str
    istr = S.fromList (show i)
    close = mkCur $ "*" <> istr <> "]*"

    collect :: Cursor -> [BufIdx]
    collect c = foldr collectf [] c

    collectf :: CursPos -> [BufIdx] -> [BufIdx]
    collectf c xs = case toRange c of
      Nothing -> xs
      Just (_,j) -> j:xs

prettySingle :: Cursor -> Int -> State -> State
prettySingle c i st = insertPrim (collect c) single st
  where
    mkCur :: Text -> Text
    mkCur str = if i == current st then
        "*" <> str <> "*"
      else
        str
    istr = S.fromList (show i)
    single = mkCur $ "*[" <> istr <> "]*"

    collect :: Cursor -> [BufIdx]
    collect c = foldr collectf [] c

    collectf :: CursPos -> [BufIdx] -> [BufIdx]
    collectf (CursPos Nothing (i,_)) xs = i:xs
    collectf _ xs = xs

prettyHelper :: Int -> State -> State
prettyHelper i st = st'''
  where
    getC i s = (cursors s) `S.index` i
    st' = prettyOpen (getC i st) i st
    st'' = prettySingle (getC i st') i st'
    st''' = prettyClose (getC i st'') i st''
