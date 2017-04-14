module WeikeHaskellUtilLib.Data.Maybe.Extension
    (
        maybeBinaryArith,
        maybeUnaryArith,
        (+?),
        (-?),
        (*?),
        (//?),
        (/?),
        maybeSqrt
    ) where

maybeBinaryArith :: (Num a) => (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybeBinaryArith f x y = do
    a <- x
    b <- y
    return (f a b)

maybeUnaryArith :: (Num a) => (a -> a) -> Maybe a -> Maybe a
maybeUnaryArith f x = do
    a <- x
    return (f a)

(+?) :: (Num a) => Maybe a -> Maybe a -> Maybe a
(+?) = maybeBinaryArith (+)

(-?) :: (Num a) => Maybe a -> Maybe a -> Maybe a
(-?) = maybeBinaryArith (-)

(*?) :: (Num a) => Maybe a -> Maybe a -> Maybe a
(*?) = maybeBinaryArith (*)

(//?) :: (Integral a) => Maybe a -> Maybe a -> Maybe a
(//?) = maybeBinaryArith div

(/?) :: (Fractional a) => Maybe a -> Maybe a -> Maybe a
(/?) = maybeBinaryArith (/)

maybeSqrt :: Maybe Double -> Maybe Double
maybeSqrt = maybeUnaryArith sqrt
