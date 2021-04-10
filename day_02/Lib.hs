module Lib
    ( someFunc
    ) where

import qualified Data.Set as Set
import Data.List
import Control.Arrow


someFunc :: IO ()
someFunc = do
        input <- (fmap lines $ readFile "input.txt")
        -- Calculate part 1
        let findNums = map (sort >>> group >>> (map length) >>> ((fromEnum . (elem 2)) &&& (fromEnum . (elem 3)))) (input)
        let (twos,threes) = foldl (\(a,b) -> ((+) a) *** ((+) b)) (0,0) findNums
        print (twos*threes)

        -- Calculate part 2
        let sortedInput = sort input
        let diffAmounts = map (length . filter (\a->a)) $ zipWith (zipWith (/=)) sortedInput (tail sortedInput)
        let results = zip diffAmounts (zip sortedInput (tail sortedInput))
        let finalLine = snd . head $ filter ( (==1) . fst ) results
        print $ zipWith (\a b -> if a == b then a else '*') (fst finalLine)  (snd finalLine)
