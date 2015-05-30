problem_001 = sum [ x | x <- [0..999], x `mod` 5 == 0 || x `mod` 3 == 0]  

--------------------------------------------------------------------------------

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
problem_002 = sum $ filter even $ takeWhile (< 4000000) fibs 
