import System.IO
import Data.Char

path = "input.txt"
test = "example.txt"


{-
FIND THE SYMBOLS    -- done
find the numbers around the symbols
add them to a list
-}

findSymbolsInFile::[String] -> [(Int, Int)]
findSymbolsInFile file = [ ( x, y) | x <- [0..((length file) - 1)], y <- (findSymbolsInLine (file!!x) 0 [])]    -- where statements weren't working and i don't have the time to figure out why

findSymbolsInLine::String -> Int -> [Int] -> [Int]
findSymbolsInLine line i exes
                                | i == (length line) = exes
                                | (isDigit ch || ch == '.') = findSymbolsInLine line (i+1) exes
                                | otherwise = findSymbolsInLine line (i+1) (exes ++ [i])
                                where ch = line!!i

findNumbers::[String] -> (Int,Int) -> [Int] -> String -> Bool -> [Int]
findNumbers lines (i,j) current nomberstring safe
                                            | i == (length (lines!!j)) && (j+1) == length lines = (checker current nomberstring safe)
                                            | i == (length (lines!!j)) = findNumbers lines (0, (j+1)) (checker current nomberstring safe) "" False  -- this is bad code, clean later
                                            | isDigit ch = findNumbers lines ((i+1),j) current nomberstring (isConnected lines (i, j) )
                                            | otherwise = findNumbers lines ((i+1),j) (checker current nomberstring safe) "" False
                                            where ch = (lines!!j)!!i


nextWord :: [String] -> (Int,Int) -> [Int] -> String -> Bool -> Bool
                                                  
isConnected::[String] -> (Int, Int) -> Bool -- works fine
isConnected lines (i,j) = or [ ((i+x), (j+y)) == z | x <- [-1..1], y <- [-1..1], z <- (findSymbolsInFile lines)]

{-
findNumbersInLine::String -> Int -> [Int] -> String -> [Int]
findNumbersInLine line i current nomber --this is stupid, I do not know why I did this; still worth saving for later
                                | i == (length line) = checker current nomber
                                | isDigit ch = findNumbersInLine line (i+1) current (nomber++ch)
                                | ch /= '.'  = findNumbersInLine line (i+1) (checker current nomber) ""
                                | otherwise = findNumbersInLine line (i+1) (checker current nomber) ""
                                where ch = line!!i
-}


checker:: [Int] -> String -> Bool-> [Int]
checker current _ false = current
checker current nomber true = current ++ [read nomber::Int]

main = do
        {--}
        contents <- readFile test
        let burnmydread = findSymbolsInFile (lines contents)
        print burnmydread
        print (findNumbers (lines contents) (0,0) [] "" False)