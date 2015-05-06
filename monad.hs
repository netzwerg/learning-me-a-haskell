-- http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html
f, g :: Float -> Float
f x = x + 1
g x = x * 10

f', g' :: Float -> (Float, String)
f' x = ((f x), "f")
g' x = ((g x), "g")

bind :: (Float -> (Float, String)) -> ((Float, String) -> (Float, String))
bind f' (gx, gs) = let (fx, fs) = f' gx in (fx, gs ++ fs)  

composition = bind f' . g' -- composition 4 --> (41,"gf")
test = bind (\x -> (x, "st")) . (\x -> (x, "te")) -- test 2 -> (2.0,"test")
