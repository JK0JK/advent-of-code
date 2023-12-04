import System.IO
import Data.Char

path = "input.txt"
test = "example.txt"

------------------------------------------------------------------------------------------------
win::[String] -> [Int]
win line = [ read x ::Int | x <- (take 10 (drop 2 line))] -- todo: maybe make a function that checks how many to take and drop? take 5 for example, but 10 for input

play::[String] -> [Int]
play line = [ read x ::Int | x <- (drop 13 line)]

------------------------------------------------------------------------------------------------

pointer:: [Int] -> [Int]
pointer hits = [2^(x - 1) | x <- hits, x /= 0]

winner:: String -> Int
winner line = length [ x | x <- (play (words line)), x `elem` (win (words line))] --how many points won

winFile:: [String] -> [Int]
winFile file = [ (winner x) | x <- file]

main = do
        contents <- readFile path
        let burnmydread = pointer (winFile ((lines contents)))
        print burnmydread
        print (sum burnmydread)