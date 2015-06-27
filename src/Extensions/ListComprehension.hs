{-# LANGUAGE ParallelListComp, TransformListComp #-}

module Extensions.ListComprehension where




doubleAll :: Int -> [Int]
doubleAll n = [x + y | x <-[1..n] | y <- [n, n-1..1], then reverse]

