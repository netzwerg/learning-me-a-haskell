-- http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html

import Control.Monad.Writer

--------------------------------------------------------------------------------

f, g :: Float -> Float
f x = x + 1
g x = x * 10

f', g' :: Float -> (Float, String)
f' x = ((f x), "f")
g' x = ((g x), "g")

bind :: (Float -> (Float, String)) -> ((Float, String) -> (Float, String))
bind f' (gx, gs) = let (fx, fs) = f' gx in (fx, gs ++ fs)  

composed = bind f' . g' -- composed 4 --> (41,"gf")
test = bind (\x -> (x, "st")) . (\x -> (x, "te")) -- test 2 -> (2.0,"test")

--------------------------------------------------------------------------------

three, five :: Int -> [Int]
three x = take 3 (repeat x)
five x = take 5 (repeat x)

flatMap :: (Int -> [Int]) -> [Int] -> [Int]
flatMap f x = concat (map f x) 
fifteen = flatMap three . five

fifteen' x = do 
                y <- five x
                z <- three y
                return z

--------------------------------------------------------------------------------

f'', g'' :: Float -> Writer String Float  
f'' x = writer (x + 1,  "f")
g'' x = writer (x * 10, "g")

-- runWriter $ composed' 5 --> (41.0,"gf")
composed' x = do
                y <- g'' x
                z <- f'' y
                return z
        
