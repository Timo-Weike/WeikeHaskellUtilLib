module WeikeHaskellUtilLib.Data.Lists.Sorting
    (
        runSort
    ) where

runs :: (Ord a) => [a] -> [[a]]
runs []       = [[]]
runs [a]      = [[a]]
runs (a:b:xs) = if a <= b then  ascRun [a] b xs
                          else descRun [a] b xs
    where
        ascRun  as b []     = [reverse (b:as)]
        ascRun  as b (x:xs) = if b <= x then ascRun (b:as) x xs
                                       else (reverse (b:as)):(runs (x:xs))
        descRun as b []     = [(b:as)]
        descRun as b (x:xs) = if b <= x then (b:as):(runs (x:xs))
                                        else descRun (b:as) x xs


runSort :: (Ord a) => [a] -> [a]
runSort as = listMerge (runs as) []
    where
        merge :: (Ord a) => [a] -> [a] -> [a]
        merge a [] = a
        merge [] b = b
        merge r@(a:as) l@(b:bs) | a <= b    = a:merge as l
                                | otherwise = b:merge r bs
        listMerge :: (Ord a) => [[a]] -> [[a]] -> [a]
        listMerge [] a = if length a > 1 then listMerge a [] else head a
        listMerge [a] b = listMerge [] (a:b)
        listMerge (a:b:as) c = listMerge as ((merge a b):c)
