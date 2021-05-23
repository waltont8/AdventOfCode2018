{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where


import Data.Text as T (pack)
import Data.List
import Data.Ord
import Data.Either
import Control.Arrow
import Control.Applicative
import Text.ParserCombinators.ReadP
import qualified Data.Map as M

-- Data types to hold the guard data
data EventType = Wake | Sleep | Start | End deriving (Show, Eq)
data Line = Line Int Int Int Int EventType (Maybe Int) deriving (Show, Eq)
data Event = Event {gnum, smin, emin :: Int} deriving (Show, Eq, Ord)

instance Ord Line where
    compare (Line m d h mi e g) (Line m' d' h' mi' e' g')
        | cm /= EQ = cm
        | cd /= EQ = cd
        | ch /= EQ = ch
        | cmi /= EQ = cmi
        | otherwise = EQ
        where
            cm = compare m m'
            cd = compare d d'
            ch = compare h h'
            cmi = compare mi mi'
                                                            
-- Simple ReadP parser
eventRead "wakes" = Wake
eventRead "falls" = Sleep
eventRead "Guard" = Start

numbers :: Int -> ReadP Int
numbers digits =
    fmap read (count digits digit)

digit :: ReadP Char
digit =
    satisfy (\char -> char >= '0' && char <= '9')

guardnumParser :: ReadP Int
guardnumParser = do
    string " #"
    n <- fmap read (count 4 digit) <|> fmap read (count 3 digit)
    return n

inputline :: ReadP Line
inputline = do
    string "[1518-"
    month <- fmap read (count 2 digit)
    satisfy (== '-')
    day <- fmap read (count 2 digit)
    satisfy (== ' ')
    hour <- fmap read (count 2 digit)
    satisfy (== ':')
    minute <- fmap read (count 2 digit)
    string "] "
    e <- fmap eventRead (string "falls") <|> fmap eventRead (string "Guard") <|> fmap eventRead (string "wakes") 
    gNum <- option Nothing (fmap Just guardnumParser)
    return (Line month day hour minute e gNum)

-- Turn a parsed line into an event
toEvent :: [Line] -> [Event]
toEvent l = iToEvent l 0 0
    where
        iToEvent ((Line _ _ _ _ Start (Just g)):xs) gnum start = iToEvent xs g 0
        iToEvent ((Line _ _ _ m Sleep _):xs) gnum start = iToEvent xs gnum m
        iToEvent ((Line _ _ _ m Wake _):xs) gnum start = (Event gnum start m) : (iToEvent xs gnum m)
        iToEvent ((Line _ _ _ _ End _):xs) gnum start = (iToEvent xs gnum start)
        iToEvent [] _ _ = []
        iToEvent l g s = error $ show l

counter :: [Event] -> (M.Map (Int, Int) Int)
counter e = c e M.empty :: M.Map (Int,Int) Int
    where
        c ((Event gNum mStart mEnd):xs) eMap = c xs $ foldl (\e n -> mapInc e (gNum,n)) eMap [mStart .. mEnd-1]
        c [] eMap = eMap

summary :: [((Int, Int), Int)] -> (M.Map Int Int, M.Map Int Int)
summary m = summary' m (M.empty :: M.Map Int Int) (M.empty :: M.Map Int Int)
  where
    summary' (((g,m),c):xs) gMap mMap = summary' xs (mapAdd gMap g c) (mapAdd mMap m c)
    summary' [] gMap mMap = (gMap, mMap)

someFunc :: IO ()
someFunc = do
    input <- (fmap lines $ readFile "input.txt")
    let j = map (fst . last . (readP_to_S inputline)) input
    let gSleepMins = (sort >>> toEvent >>> counter) j
    let (gRes, mRes) = summary $ M.toList gSleepMins
    let sleepyGuard = (sortBy (comparing snd) >>> reverse >>> head >>> fst) $ M.toList gRes
    let sleepyGuardMinute = (filter (\((a,b),c) -> a == sleepyGuard) >>> sortBy (comparing snd) >>> reverse >>> head >>> (snd . fst)) $ M.toList gSleepMins
    print $ sleepyGuard * sleepyGuardMinute
    putStrLn $ show $ (sortBy (comparing snd) >>> last >>> fst >>> (fst &&& snd) >>> uncurry (*) ) $ M.toList gSleepMins







-- Add to or increment a map
mapInc :: Ord a => (M.Map a Int) -> a -> (M.Map a Int)
mapInc m a = if (M.member a m) then (M.insert a ((m M.! a) + 1) m) else (M.insert a 1 m)

mapAdd :: Ord a => (M.Map a Int) -> a -> Int -> (M.Map a Int)
mapAdd m a i = if (M.member a m) then (M.insert a ((m M.! a) + i) m) else (M.insert a i m)

mapRead :: Ord a => (M.Map a Int) -> a -> Int
mapRead m a = if (M.member a m) then (m M.! a) else 0

