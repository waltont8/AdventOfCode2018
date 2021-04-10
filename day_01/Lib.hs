module Lib
    ( someFunc
    ) where

import qualified Data.Set as Set
import Data.List
import Control.Arrow


someFunc :: IO ()
someFunc = do
        input <- (fmap lines $ readFile "input.txt")
        let nums = map readSignedInt input
        print $ sum nums
        print $ firstRepeat $ rollingSums (cycle nums)


-- Miscellaneous
firstRepeat :: Ord a => [a] -> Maybe a
firstRepeat xs = firstRepeat' xs Set.empty
    where firstRepeat' [] _ = Nothing
          firstRepeat' (x:xs) s = if Set.member x s
                                     then Just x
                                     else firstRepeat' xs (Set.insert x s)

rollingSums :: [Integer] -> [Integer]
rollingSums (h:xs) = h : (rollingSums' h xs)
    where
        rollingSums' _ [] = [] 
        rollingSums' ret (h:xs) = (ret+h) : (rollingSums' (ret+h) xs)


-- Conversion functions?
readSignedInt :: String -> Integer
readSignedInt (h:xs) = case h of
                    '-' -> ((read xs)::Integer) * (-1)
                    '+' -> (read xs)::Integer
                    otherwise -> error "+/- passed to readSignedInt"
