import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
--------------------------------------------------------------------------------

type Log = [String]
data Logger a = Logger { logger :: (a, Log) }

record :: String -> Logger ()
record s = Logger ((), [s])

escape :: Logger a -> (a, Log)
escape (Logger (l, r)) = (l, r)

instance Monad Logger where
    (>>=) m f = let (a, w) = logger m
                    n = f a
                    (b, x) = logger n
              in Logger (b, w ++ x)
    return a = Logger (a, [])

--------------------------------------------------------------------------------
-- Fix warning 'Logger is an instance of Monad but not Applicative'

instance Functor Logger where
    fmap = liftM

instance Applicative Logger where
    pure = return
    (<*>) = ap

--------------------------------------------------------------------------------

f :: (Num a) => a -> Logger a
f x = do
    record "f called"
    return (x + 1)

g :: (Num a) => a -> Logger a
g x = do
    record "g called"
    return (x + 2)

exec :: (Num a) => a -> Logger a
exec x = do
    y <- f x
    z <- g y
    return z

--------------------------------------------------------------------------------

main = putStrLn $ show $ escape $ exec 42
