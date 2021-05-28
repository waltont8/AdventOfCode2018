module Lib
    ( someFunc
    ) where

import Control.Arrow
import Data.List
import Data.Char

someFunc :: IO ()
someFunc = interact $
{-- part 1 --}--lines >>> (!!0) >>> crunch >>> length >>> show
{-- part 2 --}  lines >>> (!!0) >>> manyToOne (map removeAChar ['a'..'z']) >>> map crunch >>> map length >>> minimum >>> show

crunch :: String -> String
crunch a = if a == a' then a else crunch a' 
    where 
        a' = polarMatch a
        polarMatch (a:b:c) = if (a /= b) && ((toLower a) == (toLower b)) then
                                polarMatch c
                             else
                                a:(polarMatch (b:c))
        polarMatch [a] = [a]
        polarMatch [] = []

removeAChar c = filter (\x -> toLower(x) /= toLower(c))


manyToOne fs x = map (\f -> f x) fs
