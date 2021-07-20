import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let n = read input_line :: Int -- the number of temperatures to analyse
    input_line <- getLine
    let input = words input_line
    let list = [(read (input!!i) :: Int) | i <- [0..(n-1)]]
    let result = if null list then "0" else show $ foldl1 findMin list
    putStrLn result
    return ()
        where findMin acc x
                | abs x < abs acc = x
                | abs x == abs acc = max acc x
                | otherwise = acc