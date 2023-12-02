import Data.Char
import System.IO
import Data.Typeable
import Text.Regex as R
import qualified Data.List as L

readLine::[Char] -> [Char]
readLine line = [x | x <- line, isDigit x]

replaceAllIn :: String -> [(String, String)] -> String
replaceAllIn = foldl (\acc (k, v) -> R.subRegex (R.mkRegex k) acc v)    -- problem being it detects in order of appearance on key

key = [("zero", "0"), ("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")] -- there's better ways to do this but we're under a time limit


addNumber::String -> String
addNumber [] = ""
addNumber (x:xs) = x : xs


interpret :: [Char] -> [Char]
interpret line = (head y) : (last y) : []   -- it's weird that you'd need `: []` at the end, you'd expect `:` to be enough
                where y = readLine line

readLines:: String -> [Int]
readLines lien = [read (interpret x) | x <- lion]
                where lion = lines (replaceAllIn lien key)

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

