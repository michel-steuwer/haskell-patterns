-- | Main entry point to the application.
module Main where

import Patterns

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"
    print (scal 1.5 [1.0, 2.0, 3.0, 4.0, 5.0])
    
    print (asum [1.5, 2.5, -3.5, 4.5])
    
    print (dot [1,2,3,4,5,6,7,8,9] [9,8,7,6,5,4,3,2,1])
    
    print (gemv [[1,2,3],[4,5,6],[7,8,9]] [42,42,42] [0,0,0] 1.5 2.5)

    print (split 2 [1,2,3,4])

    print (join [[1,2],[3,4]])

    print (Patterns.iterate 7 (\ xs -> '-' : xs) "")

    print (vectorScale1 (take 8 (Prelude.iterate succ 1)))

    print (vectorScale2 (take 8 (Prelude.iterate succ 1)))

    print (vectorScale3 (take 8 (Prelude.iterate succ 1)))

    print (vectorScale4 (take 8 (Prelude.iterate succ 1)))

    print (vecSum0 (take 128 (Prelude.iterate succ 1)))

    print (vecSum1 (take 256 (Prelude.iterate succ 1)))

    print (vecSum2 (take 1024 (Prelude.iterate succ 1)))

    print (vecSum3 (take 1024 (Prelude.iterate succ 1)))

    print (vecSum4 (take 1024 (Prelude.iterate succ 1)))

    print (vecSum5 (take 262144 (Prelude.iterate succ 1)))