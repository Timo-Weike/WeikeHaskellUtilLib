module WeikeHaskellUtilLib.Data.Lists.Managing
    (
        insertAt,
        slice,
        stdShuffle,
        shuffle
    ) where

import System.Random

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 a b = a:b
insertAt _ a []    = [a]
insertAt i a (b:bs) = b:(insertAt (i-1) a bs)

slice :: [a] -> Int -> Int -> [a]
slice a l r = take (r-l) $ drop l a

shuffle :: Int -> [a] -> [a]
shuffle initR a = shuffle' a [] r
    where
        l = length a
        r = mkStdGen initR
        shuffle' [] a _ = a
        shuffle' (a:as) b r = shuffle' as (insertAt (mod ins l) a b) newR
            where
                (ins, newR) = next r


stdShuffle :: [a] -> [a]
stdShuffle = shuffle 2137
