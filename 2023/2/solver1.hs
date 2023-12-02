import System.IO
import Text.Regex as R

solver::String -> [Int] -> Int
solver lineIn values = sum (listo (lines lineIn) values)

lista::String -> [Int] -> [Int]   -- todo remove
lista lineIn values = listo (lines lineIn) values

listo::[String] -> [Int] -> [Int]
listo listStr values = [ x | x <- [1..(length listStr)], isGood (listStr!!(x-1)) values]

isGood::String -> [Int] -> Bool
isGood x values = compValues (parseLine (drop 2 (words x)) [0,0,0]) values

compValues:: [Int] -> [Int] -> Bool
compValues (a:b:c:_) (d:e:f:_) = a <= d && b <= e && c <= f

parseLine::[String] -> [Int] -> [Int]
parseLine [] woop = woop
parseLine (x:y:xs) [r,g,b] 
                           | abo == "r" = killme [r+val, g, b]
                           | abo == "g" = parseLine xs [r, g+val, b]
                           | abo == "b" = parseLine xs [r, g, b+val]
                           where abo = take 1 y
                                 val = read x ::Int
                                 killme = parseLine xs    --later

path = "input.txt"
vals = [12,13,14]

inpoot1 = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
results = [1,2,5]

main = do
        {--}
        contents <- readFile path
        let numba = solver contents vals --todo change lista to solver
        print numba
        print (solver inpoot1 vals)
        print (sum results)
        {--}