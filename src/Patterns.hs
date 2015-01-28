{-# LANGUAGE RankNTypes #-}
module Patterns where

import Prelude hiding (map, zip, iterate)
import qualified Prelude (map, zip, iterate)

import Numeric.SpecFunctions (log2)
import Data.Array

type Vec = Array Int

-- high-level patterns
map :: forall a b. (a -> b) -> [a] -> [b]
map = Prelude.map

reduce :: forall a. (a -> a -> a) -> a -> [a] -> [a]
reduce f z xs = [Prelude.foldl f z xs]

zip :: forall a b. [a] -> [b] -> [(a,b)]
zip = Prelude.zip

split :: forall a. Int -> [a] -> [[a]]
split n xs | len `mod` n == 0   = builder (len `div` n) xs
           | otherwise          = error "Length not divisible by n"
    where len = length xs
          builder 1 xs = [take n xs]
          builder i xs = take n xs : builder (i-1) (drop n xs)

join :: forall a. [[a]] -> [a]
join = foldl (++) []

iterate :: forall a. Int -> (a -> a) -> a -> a
iterate 0 f xs = xs
iterate n f xs = iterate (n-1) f (f xs)

reorder :: forall a. [a] -> [a]
reorder = id

-- low-level patterns
mapWorkgroup    = map
mapLocal        = map
mapGlobal       = map
mapWarp         = map
mapLane         = map
mapSeq          = map

reduceSeq :: forall a b. (a -> b -> a) -> a -> [b] -> [a]
reduceSeq f z xs = [Prelude.foldl f z xs]

reorderStride :: forall a. Int -> [a] -> [a]
reorderStride s = id

toLocal :: forall a b. (a -> b) -> (a -> b)
toLocal = id

toGlobal :: forall a b. (a -> b) -> (a -> b)
toGlobal = id

asVector :: forall a. Int -> [a] -> [Vec a]
asVector n xs | len `mod` n == 0    = builder (len `div` n) xs
              | otherwise           = error "Length not divisible by n"
    where len = length xs
          builder 1 xs = [newVector (take n xs)]
          builder i xs = newVector (take n xs) : builder (i-1) (drop n xs)
          newVector xs = array (1, n) (zip [1..n] xs)

asScalar :: forall a. [Vec a] -> [a]
asScalar = join . map elems

vectorize :: forall a b. Int -> (a -> b) -> (Vec a -> Vec b)
vectorize n f = \ a -> array (bounds a) $ map (\ (i, e) -> (i, f e)) $ assocs a

-- utilities
onPairs :: forall a b c. (a -> b -> c) -> ((a, b) -> c)
onPairs f = \ (a, b) -> f a b


-- benchmarks
scal :: forall a. Num a => a -> [a] -> [a]
scal a = map ((*) a)

asum :: forall a. (Ord a, Num a) => [a] -> [a]
asum xs = reduce (+) 0 (map abs xs)

dot :: forall a. Num a => [a] -> [a] -> [a]
dot xs ys = reduce (+) 0 (map (onPairs (*)) (zip xs ys))

gemv :: forall a. Num a => [[a]] -> [a] -> [a] -> a -> a -> [a]
gemv mss xs ys a b = map (onPairs (+)) (zip zs (scal b ys))
    where zs = map (head . scal a . dot xs) mss


-- vector scale
mult3 :: forall a. Num a => a -> a
mult3 x = x * 3

vectorScale1 :: forall a. Num a => [a] -> [a]
vectorScale1 = map mult3

vectorScale2 :: forall a. Num a => [a] -> [a]
vectorScale2 xs = join . map (map mult3) . split (s `div` 2) $ xs
    where s = length xs

vectorScale3 :: forall a. Num a => [a] -> [a]
vectorScale3 xs = join . map (
        asScalar . map (vectorize 4 mult3) . asVector 4
    ) . split (s `div` 2) $ xs
    where s = length xs

vectorScale4 :: forall a. Num a => [a] -> [a]
vectorScale4 xs = join . mapWorkgroup (
        asScalar . mapLocal (vectorize 4 mult3) . asVector 4
    ) . split (s `div` 2) $ xs
    where s = length xs




-- reduction
vecSum0 = reduce (+) 0

vecSum1 xs = vecSum0 . join . mapWorkgroup (
        join . toGlobal (mapLocal (mapSeq id)) . split 1 .
        iterate (log2 wgSize) (
            join . mapLocal (reduceSeq (+) 0) . split 2
        ) .
        join . toLocal (mapLocal (mapSeq id)) . split 1
    ) . split wgSize $ xs
    where wgSize = 128

vecSum2 xs = vecSum0 . join . mapWorkgroup (
        join . toGlobal (mapLocal (mapSeq id)) . split 1 .
        iterate (log2 wgSize) (
            join . mapLocal (reduceSeq (+) 0) . split 2
        ) .
        join . toLocal (mapLocal (reduceSeq (+) 0)) . split 2
    ) . split (2 * wgSize) $ xs
    where wgSize = 128

vecSum3 xs = vecSum0 . join . mapWorkgroup (
        join . toGlobal (mapLocal (mapSeq id)) . split 1 .
        join . mapWarp (
            join . mapLane (reduceSeq (+) 0) . split 2 .
            join . mapLane (reduceSeq (+) 0) . split 2 .
            join . mapLane (reduceSeq (+) 0) . split 2 .
            join . mapLane (reduceSeq (+) 0) . split 2 .
            join . mapLane (reduceSeq (+) 0) . split 2 .
            join . mapLane (reduceSeq (+) 0) . split 2
        ) . split 64 .
        iterate (log2 wgSize - log2 64) (
            join . mapLocal (reduceSeq (+) 0) . split 2
        ) .
        join . toLocal (mapLocal (reduceSeq (+) 0)) . split 2
    ) . split (2 * wgSize) $ xs
    where wgSize = 128

vecSum4 xs = vecSum0 . join . mapWorkgroup (
        join . toGlobal (mapLocal (mapSeq id)) . split 1 .
        join . mapWarp (
            join . mapLane (reduceSeq (+) 0) . split 2 .
            join . mapLane (reduceSeq (+) 0) . split 2 .
            join . mapLane (reduceSeq (+) 0) . split 2 .
            join . mapLane (reduceSeq (+) 0) . split 2 .
            join . mapLane (reduceSeq (+) 0) . split 2 .
            join . mapLane (reduceSeq (+) 0) . split 2
        ) . split 64 .
        join . mapLocal (reduceSeq (+) 0) . split 2 .
        join . toLocal (mapLocal (reduceSeq (+) 0)) . split 2
    ) . split (2 * wgSize) $ xs
    where wgSize = 128

vecSum5 xs = vecSum0 . join . mapWorkgroup (
        join . toGlobal (mapLocal (mapSeq id)) . split 1 .
        join . mapWarp (
            join . mapLane (reduceSeq (+) 0) . split 2 .
            join . mapLane (reduceSeq (+) 0) . split 2 .
            join . mapLane (reduceSeq (+) 0) . split 2 .
            join . mapLane (reduceSeq (+) 0) . split 2 .
            join . mapLane (reduceSeq (+) 0) . split 2 .
            join . mapLane (reduceSeq (+) 0) . split 2
        ) . split 64 .
        join . mapLocal (reduceSeq (+) 0) . split 2 .
        join . toLocal (mapLocal (reduceSeq (+) 0)) . 
            split (blockSize `div` wgSize)
    ) . split blockSize $ xs
    where wgSize = 128
          blockSize = 262144

