{-# LANGUAGE BlockArguments #-}

import Data.Char
import System.Random
import Control.Monad

data BOps = AddOp | SubOp
  deriving Show

data COps = LTOp | GTOp | LTEOp | GTEOp
  deriving Show

data Token = BOp BOps | COp COps | Die Int | CSym Int | RolledDie (IO Int)

preproc :: String -> String
preproc "" = ""
preproc ('+':xs)     = ' ' : '+' : ' ' : preproc xs
preproc ('-':xs)     = ' ' : '-' : ' ' : preproc xs
preproc ('<':'=':xs) = ' ' : '<' : '=' : ' ' : preproc xs
preproc ('>':'=':xs) = ' ' : '>' : '=' : ' ' : preproc xs
preproc ('<':xs)     = ' ' : '<' : ' ' : preproc xs
preproc ('>':xs)     = ' ' : '>' : ' ' : preproc xs
preproc ('d':xs)     = ' ' : 'd' : ' ' : preproc xs
preproc (x:xs) = x : preproc xs

lexer :: String -> [Token]
lexer = analyze . words . preproc

analyze :: [String] -> [Token]
analyze []        = []
analyze ("+":xs)  = BOp AddOp : analyze xs
analyze ("-":xs)  = BOp SubOp : analyze xs
analyze ("<":xs)  = COp LTOp  : analyze xs
analyze (">":xs)  = COp GTOp  : analyze xs
analyze ("<=":xs) = COp LTEOp : analyze xs
analyze (">=":xs) = COp GTEOp : analyze xs
analyze ("d":x:xs) | isNum x = Die (read x :: Int) : analyze xs
analyze (x:xs) | isNum x = CSym (read x :: Int) : analyze xs
analyze s = error $ "Token error: " ++ concat s

--helper fucntion(s)--
isNum :: String -> Bool
isNum ""  = False
isNum xs =
  case dropWhile isDigit xs of
    ""       -> True   --all characters were numbers
    _        -> False  --default case (anything else)

--main functions--
evaluate :: [Token] -> IO ()
evaluate [] = return()
evaluate (CSym c:Die d:xs) = if c > 1 then evaluate (CSym (c-1):Die d:BOp AddOp:(RolledDie (rollDice d):xs))
                                      else evaluate (RolledDie (rollDice d):xs)
evaluate (RolledDie v:BOp op:Die d:xs) = evaluate (RolledDie v:BOp op:RolledDie (rollDice d):xs)
evaluate (Die d:BOp op:RolledDie v:xs) = evaluate (RolledDie (rollDice d):BOp op:RolledDie v:xs)
evaluate (RolledDie v1:BOp AddOp:RolledDie v2:xs) = evaluate (RolledDie (diceMath (+) v1 v2):xs)
evaluate (RolledDie v1:BOp SubOp:RolledDie v2:xs) = evaluate (RolledDie (diceMath (-) v1 v2):xs)
evaluate (Die d:xs) = evaluate ((RolledDie (rollDice d)):xs)
evaluate (RolledDie v:BOp AddOp:CSym c:xs) = evaluate (RolledDie ((c+) <$> v):xs)
evaluate (RolledDie v:BOp SubOp:CSym c:xs) = evaluate (RolledDie ((c-) <$> v):xs)
evaluate (RolledDie v:COp LTOp:CSym c:xs) = do
  result <- v
  putStr . show $ result
  putStr " -> "
  if (result < c)
    then putStrLn "success"
    else putStrLn "failure"
evaluate (RolledDie v:COp LTEOp:CSym c:xs) = do
  result <- v
  putStr . show $ result
  putStr " -> "
  if (result <= c)
    then putStrLn "success"
    else putStrLn "failure"
evaluate (RolledDie v:COp GTOp:CSym c:xs) = do
  result <- v 
  putStr . show $ result
  putStr " -> "
  if (result > c)
    then putStrLn "Success"
    else putStrLn "Failure"
evaluate (RolledDie v:COp GTEOp:CSym c:xs) = do
  result <- v
  putStr . show $ result
  putStr " -> "
  if (result >= c)
    then putStrLn "success"
    else putStrLn "failure"
evaluate (RolledDie v:xs) = do 
  output <- v
  print output
evaluate (x:xs) = evaluate xs

rollDice :: Int -> IO Int
rollDice d = do
  randomRIO(1,d)

diceMath :: (Int -> Int -> Int) -> IO Int -> IO Int -> IO Int
diceMath f v1 v2 = do
  x <- v1
  y <- v2
  return (f x y)

usage :: IO ()
usage = do
  putStrLn "----------------------------------------Dice Roller-----------------------------------------"
  putStrLn "Usage: "
  putStrLn " > to roll a die first enter the command \"roll\" and then wait for the prompt."
  putStrLn "   then enter the number of dice, the letter \"d\", and the number of faces on the die."
  putStrLn "   e.g. 2d10 is two, ten-sided dice"
  putStrLn " > optionally you can then type \"+\" or \"-\" followed by a number to add/subtract from the"
  putStrLn "   result of the die."
  putStrLn "   e.g. d20+3 is the result of a twenty-sided die plus three"
  putStrLn " > optionally you can compare the result to a target value using the \"<\" \">\" \"<=\" or"
  putStrLn "   \">=\" symbols."
  putStrLn "   e.g. d20+7 > 15 compares the result of a twenty-sided die plus seven to 15 using the "
  putStrLn "   \"greater-than\" operator."
  putStrLn "--------------------------------------------------------------------------------------------"

roll :: IO ()
roll = do
  putStrLn "Enter Dice:"
  input <- getLine
  putStr "rolling ["
  putStr input
  putStr "] -> "  
  evaluate (lexer input)