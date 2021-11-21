import Data.List
import Data.List.Split
import Data.Maybe

allPass min max n = n >= min && n <= max
parseInt = read::String-> Int

parseAnswers (min:max:letter:rest:_) = (answer1, answer2)
  where 
    minimum = parseInt min
    maximum = parseInt max
    ltr = head letter
    c = filter (\(_, idx) -> idx + 1 ==  minimum || idx + 1 == maximum) $ zip rest [0..]
    filtered = filter (\(val, _) -> val == ltr) c
    len = length $ filtered
    answer1 = allPass minimum maximum . length $ filter (\(val) -> val == ltr) rest
    answer2 = (==1) $ len

main = 
  do
    contents <- readFile "input.txt" 
    let inputData = map parseAnswers $ map words $ lines contents
    let answer1 = length $ filter (\(answer1, answer2) -> answer1 == True) inputData
    let answer2 = length $ filter (\(answer1, answer2) -> answer2 == True) inputData

    print (answer1)
    print (answer2)
