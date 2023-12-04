import Data.Char
import System.IO
import Data.Typeable
import qualified Data.List as L

readLine::[Char] -> [Char]
readLine line = [x | x <- line, isDigit x]

key = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] -- there's better ways to do this but we're under a time limit

replaceInLine:: (String, [Int], [Int]) -> (String, [Int],[Int])
replaceInLine ((x:xs), index, accumulator) = ("", [], [])

{-
x:xs
check if x is element index in key for every element in key ( x == key[i][index[i]] )
        if yes, increment in index ( index[i]++ )
                if index[i] == key[i].length
                        index[i] = 0
                        accumulator++i
        else, reset index (index[i] = 0)
if isDigit x, accumulator ++ (read x::Int)
do the same for xs
-}


{-

-}






interpret :: [Char] -> [Char]
interpret line = (head y) : (last y) : []   -- it's weird that you'd need `: []` at the end, you'd expect `:` to be enough
                where y = readLine line

readLines:: String -> [Int]
readLines lien = [read (interpret x) | x <- lion]
                where lion = lines (lien)

path = "input.txt"

test1 = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
results1 = [12,38,15,77]

test2 = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"
results2 = [29, 83, 13, 24, 42, 14, 76]

main = do
        {--}
        contents <- readFile path
        let numba = readLines contents
        print (sum numba)
        print (readLines test1)  -- to make sure
        print ((readLines test1) == results1)
        -- test 1 must pass
        print (readLines test2)
        print ((readLines test2) == results2)
        {--}

