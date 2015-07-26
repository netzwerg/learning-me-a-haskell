module Oopsie where

import System.IO
import System.Random
import Control.Exception
import Data.Char

-- | 
-- | Oopsie is a very simple single dice game: 
-- | 
-- | You are allowed to roll the dice as many times as you like. As long
-- | as you don't roll a 3, all points are summed up and you can continue.
-- | If however you roll a 3, you get a total of 0 ("Oopsie"). 
-- |
-- | Try maximizing a grand total over 3 rounds.
-- |
playOopsie :: IO ()
playOopsie = do
    total <- play 0 0
    putStrLn ("\n=============")
    putStrLn ("TOTAL " ++ (show $ total))
    putStrLn ("=============\n")

play :: Int -> Int -> IO Int
play round total
    | round >= 3 = return total
    | otherwise      = do
        putStrLn ("\n-------------")
        putStrLn ("Round " ++ show (round + 1))
        putStrLn ("-------------\n")
        putStrLn ("Grand Total: " ++ show total) 
        roundTotal <- playRound 0
        play (round + 1) (total + roundTotal)

playRound :: Int -> IO Int
playRound total = do
    putStrLn ("Round Total: " ++ show total) 
    putStr   ("\nDare rolling a dice (Y) or bring home (ANY OTHER KEY)? ") 
    char <- getChar
    putStrLn ""
    case (toUpper char) of
        'Y'       -> do 
                 roll <- rollDice
                 case roll of 
                     Count a -> playRound (total + a)
                     Oopsie -> return 0
        otherwise -> return total 

data Roll = Count Int | Oopsie

rollDice :: IO Roll 
rollDice = do
    number <- randomRIO (1, 6) :: IO Int 
    case number of
        3         -> do 
            putStrLn ("\n    ------------")
            putStrLn ("   | ! OOPSIE ! |")
            putStrLn ("    ------------\n")
            hSetEcho stdin False
            getChar
            hSetEcho stdin False
            return Oopsie 
        otherwise -> do
            putStrLn ("\n        ---")
            putStrLn ("       | " ++ show number ++ " |") 
            putStrLn ("        ---\n")
            return $ Count number
