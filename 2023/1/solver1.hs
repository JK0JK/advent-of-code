import Data.Char
import System.IO
import Data.Typeable

readLine::[Char] -> [Char]
readLine line = [x | x <- line, isDigit x]

interpret::[Char] -> [Char]
interpret line = (head y) : (last y) : []   -- it's weird that you'd need `: []` at the end, you'd expect `:` to be enough
                where y = readLine line

readLines:: String -> [Int]
readLines lien = [read (interpret x) | x <- lion]
                where lion = lines lien

path = "input.txt"

test = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
results = [12,38,15,77]

main = do
        contents <- readFile path
        let numba = readLines contents
        print (sum numba)
        print (readLines test)  -- to make sure
        print ((readLines test) == results)
        
