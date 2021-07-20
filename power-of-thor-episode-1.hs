import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    -- ---
    -- Hint: You can use the debug stream to print initialTX and initialTY, if Thor seems not follow your orders.
    
    input_line <- getLine
    let input = words input_line
    let lightx = read (input!!0) :: Int -- the X position of the light of power
    let lighty = read (input!!1) :: Int -- the Y position of the light of power
    let initialtx = read (input!!2) :: Int -- Thor's starting X position
    let initialty = read (input!!3) :: Int -- Thor's starting Y position
    
    -- game loop
    loop (initialtx, initialty) (lightx, lighty)

xNextDirect x1 x2
    | x1 > x2 = "W"
    | x1 < x2 = "E"
    | otherwise = ""

yNextDirect y1 y2
    | y1 > y2 = "N"
    | y1 < y2 = "S"
    | otherwise = ""

newPoint point direction
    | direction == "N" || direction == "W" = point - 1
    | direction == "S" || direction == "E" = point + 1
    | otherwise = point

loop :: (Int, Int) -> (Int, Int) -> IO ()
loop (x1, y1) (x2, y2) = do

    -- hPutStrLn stderr "Debug messages..."
        
    -- A single line providing the move to be made: N NE E SE S SW W or NW

    let xDir = xNextDirect x1 x2
    let yDir = yNextDirect y1 y2

    putStrLn $ yDir ++ xDir

    loop (newPoint x1 xDir, newPoint y1 yDir) (x2, y2)