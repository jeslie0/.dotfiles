import           Data.Char
import           Data.List
import           System.Process

monitorExtract :: String -> Char
monitorExtract = firstNum . concat . filter (isSubsequenceOf "focused") . lines


firstNum :: String -> Char
firstNum "" = 'a'
firstNum (x:xs)
  | isDigit x = x
  | otherwise = firstNum xs

main = do
  let command = shell "swaymsg -s $SWAYSOCK -t get_outputs -p"
  swayOutput <- readCreateProcess command ""
  putStr [monitorExtract swayOutput]
