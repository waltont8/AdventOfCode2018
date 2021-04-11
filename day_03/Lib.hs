{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Data.Text as T (pack)
import Data.List
import Data.Either
import Control.Arrow
import Data.Attoparsec.Text as P

data InputLine = InputLine Int Int Int Int Int deriving (Show)

inputParser :: Parser InputLine
inputParser = InputLine <$  char '#'
                        <*> decimal
                        <*  skipSpace
                        <*  char '@'
                        <*  skipSpace
                        <*> decimal
                        <*  char ','
                        <*> decimal
                        <*  char ':'
                        <*  skipSpace
                        <*> decimal
                        <*  string "x"
                        <*> decimal

inputLineToList :: InputLine -> [(Int, Int)]
inputLineToList (InputLine idx x y w h) = [ (a,b) | a <- [x..(x+w-1)], b <- [y..(y+h-1)]]


someFunc :: IO ()
someFunc = do
        input <- (fmap lines $ readFile "input.txt")
        let rects = map (fromRight (InputLine 0 0 0 0 0)) $ map ((parseOnly inputParser) . T.pack) input
        let pointList = map inputLineToList rects
        print $ (concat >>> sort >>> group >>> (map length) >>> (filter (>1)) >>> length) pointList
        let db = (concat >>> sort) pointList
        print $ findTheSquare rects db

findTheSquare (h:xs) db 
    | (any hitPoint thisRect) == True = findTheSquare xs db
    | otherwise = h
    where
        thisRect = inputLineToList h
        hitPoint (x,y) = if length (filter (==(x,y)) db) > 1 then True else False

