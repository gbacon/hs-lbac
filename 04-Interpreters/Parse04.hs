module Parser04 where

-- Part III: MORE EXPRESSIONS
-- https://xmonader.github.io/letsbuildacompiler-pretty/tutor04_interpreters.html

import Control.Monad (forM_)
import Control.Monad.Except
import Control.Monad.State
import Data.Char
import qualified Data.Map as M

type Parser04State = (Char, String, M.Map String Integer, [String])
type Parser04 = State Parser04State
type SimpleParser a = ExceptT String Parser04 a

parserValue :: String
parserValue = "!Value"

main :: IO ()
main = do
  -- input <- getContents
  let input = " Abcd= ( ( 4 / 2 )* 3) -1\n" ++
              " !aBcd\n" ++
              " abCd =AbcD *2\n" ++
              " ! abcD\n" ++
              "   . \n"
  let initialState = (' ', input, M.fromList [(parserValue, 0)], [])
  let (a,(_,_,_,output)) = runState (runExceptT parse) initialState
  case a of
    Left msg -> putStrLn $ "Error: " ++ msg
    Right value ->
      do forM_ (reverse output) putStrLn
         print value

parse :: SimpleParser Integer
parse = initParser >> go
  where
    go = do
      (look, _, _, _) <- get
      case look of
        '!' -> outputVar
        _   -> assignment
      newline
      (look', _, _, _) <- get
      case look' of
        '.' -> getValue
        _   -> go

initParser :: SimpleParser ()
initParser = nextChar >> skipWhitespace

expected :: String -> SimpleParser a
expected msg = throwError $ msg ++ " expected"

getValue :: SimpleParser Integer
getValue = get >>= \(_, _, vars, _) -> return (vars M.! parserValue)

setValue :: Integer -> SimpleParser ()
setValue i = do
  (look, input, vars, output) <- get
  let vars' = M.insert parserValue i vars
  put (look, input, vars', output)

outputVar :: SimpleParser ()
outputVar = do
  match_ (== '!') "! for output"
  name <- getName
  (look, input, vars, output) <- get
  let value = vars M.! name
      line = name ++ " = [" ++ show value ++ "]"
  put (look, input, vars, (line:output))

assignment :: SimpleParser ()
assignment = do
  name <- getName
  match_ (== '=') "'=' in assignment"
  e <- expression
  setValue e
  (look, input, vars, output) <- get
  let vars' = M.insert name e vars
  put (look, input, vars', output)

getName :: SimpleParser String
getName = do
  (look, _, _, _) <- get
  if isAlpha look
    then do nextChar
            cs <- many isAlphaNum "alphanumeric in Name"
            skipWhitespace
            return (map toUpper (look:cs))
    else expected "leading alphabetic on Name"

expression :: SimpleParser Integer
expression = do
  (look, _, _, _) <- get
  if isAddOp look
    then go 0
    else term >>= go
  where
    go i = do
      (look, _, _, _) <- get
      case look of
        '+' -> do match_ (== '+') "+ in expression"
                  num <- term
                  go (i + num)
        '-' -> do match_ (== '-') "- in expression"
                  num <- term
                  go (i - num)
        _ -> return i

factor :: SimpleParser Integer
factor = do
  (look, _, _, _) <- get
  case look of
    '(' -> do match_ (== '(') "( in factor"
              e <- expression
              match_ (== ')') ") in factor"
              return e
    _ | isAlpha look ->
      do name <- getName
         (look', input, vars, output) <- get
         if name `M.member` vars
           then return $ (M.!) vars name
           else do let vars' = M.insert name 0 vars
                   put (look', input, vars', output)
                   return 0
    _ -> getNum

getNum :: SimpleParser Integer
getNum = do skipWhitespace
            num <- many1 isDigit "Integer"
            return (read num)

term :: SimpleParser Integer
term = factor >>= go
   where
     go i = do
       skipWhitespace
       (look, _, _, _) <- get
       case look of
         '*' -> do match_ (== '*') "*"
                   num <- factor
                   go (i * num)
         '/' -> do match_ (== '/') "/"
                   num <- factor
                   go (i `div` num)
         _   -> return i

isAddOp :: Char -> Bool
isAddOp = (`elem` ['+','-'])

match :: (Char -> Bool) -> String -> SimpleParser Char
match p what = do
  skipWhitespace
  (look, _, _, _) <- get
  if p look
    then nextChar >> skipWhitespace >> return look
    else expected what

match_ :: (Char -> Bool) -> String -> SimpleParser ()
match_ p what = match p what >> return ()

many :: (Char -> Bool) -> String -> SimpleParser String
many p what = do
  (look, _, _, _) <- get
  if p look
    then do nextChar
            cs <- many p what
            return (look:cs)
    else return ""

many1 :: (Char -> Bool) -> String -> SimpleParser String
many1 p what = do
  x <- match p what
  xs <- many p what
  return (x:xs)

nextChar :: SimpleParser ()
nextChar = do
  (_, input, vars, output) <- get
  case input of
    [] -> throwError "Premature end of input"
    (c:cs) -> put (c, cs, vars, output)

skipWhitespace :: SimpleParser ()
skipWhitespace = do
  (look, _, _, _) <- get
  if look == ' ' || look == '\t'
    then nextChar >> skipWhitespace
    else return ()

newline :: SimpleParser ()
newline = match_ (== '\n') "Newline"
