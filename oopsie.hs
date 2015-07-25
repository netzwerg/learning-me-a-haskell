import System.Random

main = do
    putStrLn "Press ENTER to play"
    getLine
    number <- randomRIO (1, 6) :: IO Int 
    putStrLn ("You got " ++ show number) 
