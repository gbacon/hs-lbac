module Parser03 where

-- Part III: MORE EXPRESSIONS
-- https://xmonader.github.io/letsbuildacompiler-pretty/tutor03_moreexpressions.html

import Control.Monad (forM_)
import Control.Monad.Except
import Control.Monad.State
import Data.Char

type Parser03State = (Char, String, [String])
type Parser03 = State Parser03State
type SimpleParser a = ExceptT String Parser03 a

main = do
  -- input <- getContents
  let input = " abcd=( ( 1/2)* 3) -4\n"
  let input = " abcd = - 3 \n"
  -- let input = " abcd = 111 + 22   \n"
  -- let input = " abcd = foo ( )  \n"
  -- let input = "  foo = bar \n"
  let initialState = (' ', input, [])
  case evalState (runExceptT (initParser >> parse)) initialState of
    Left msg -> putStrLn $ "Error: " ++ msg
    Right output -> forM_ (reverse output) print

parse :: SimpleParser [String]
parse = assignment >> newline

initParser :: SimpleParser [String]
initParser = nextChar >> skipWhitespace

expected :: String -> SimpleParser a
expected msg = throwError $ msg ++ " expected"

assignment :: SimpleParser [String]
assignment = do
  name <- getName
  match (== '=') "'=' in assignment"
  expression
  emit $ "LEA " ++ name ++ "(PC),A0"
  emit $ "MOVE D0,(A0)"

getName :: SimpleParser String
getName = do
  (look, _, _) <- get
  if isAlpha look
    then do nextChar
            cs <- many isAlphaNum "alphanumeric in Name"
            skipWhitespace
            return (map toUpper (look:cs))
    else expected "leading alphabetic on Name"

expression :: SimpleParser [String]
expression = do
  (look, _, _) <- get
  if isAddOp look
    then emit "CLR D0"
    else term
  go
  where go = do (look, _, output) <- get
                if isAddOp look
                  then emit "MOVE D0,-(SP)" >>
                       case look of
                         '+' -> add >> go
                         '-' -> sub >> go
                  else return output

factor :: SimpleParser [String]
factor = do
  (look, _, _) <- get
  case look of
    '(' -> do match (== '(') "( in factor"
              expression
              match (== ')') ") in factor"
              (_,_,output) <- get
              return output
    _ | isAlpha look -> ident
    _ -> do num <- getNum
            emit ("MOVE #" ++ num ++ ",D0")

ident :: SimpleParser [String]
ident = do
  name <- getName
  (look,_,_) <- get
  case look of
    '(' -> match (== '(') "function call" >>
           match (== ')') "() for function call" >>
           emit ("BSR " ++ name)
    _   -> emit $ "MOVE " ++ name ++ "(PC),D0"

getNum :: SimpleParser String
getNum = do num <- many1 isDigit "Integer"
            skipWhitespace
            return num

term :: SimpleParser [String]
term = factor >> go
   where go = do (look, _, output) <- get
                 if isMulOp look
                   then emit "MOVE D0,-(SP)" >>
                        case look of
                          '*' -> mult >> go
                          '/' -> divide >> go
                   else return output

add,sub,mult,divide :: SimpleParser [String]
add = do
  match (== '+') "+ in expression"
  term
  emit "ADD (SP)+,D0"

sub = do
  match (== '-') "- in expression"
  term
  emit "SUB (SP)+,D0"
  emit "NEG D0"

mult = do
  match (== '*') "* in term"
  factor
  emit "MULS (SP)+,D0"

divide = do
  match (== '/') "/ in term"
  factor
  emit "MOVE (SP)+,D1"
  emit "EXS.L D0"
  emit "DIVS D1,D0"

isAddOp :: Char -> Bool
isAddOp = (`elem` ['+','-'])

isMulOp :: Char -> Bool
isMulOp = (`elem` ['*','/'])

match :: (Char -> Bool) -> String -> SimpleParser Char
match p what = do
  skipWhitespace
  (look, _, _) <- get
  if p look
    then nextChar >> skipWhitespace >> return look
    else expected what

many :: (Char -> Bool) -> String -> SimpleParser String
many p what = do
  (look, _, _) <- get
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

emit :: String -> SimpleParser [String]
emit instr = do
  (look, input, output) <- get
  let output' = instr:output
  put (look, input, output')
  return output'

nextChar :: SimpleParser [String]
nextChar = do
  (_, input, output) <- get
  case input of
    [] -> throwError "Premature end of input"
    (c:cs) -> put (c, cs, output) >> return output

skipWhitespace :: SimpleParser [String]
skipWhitespace = do
  (look, input, output) <- get
  if look == ' ' || look == '\t'
    then nextChar >> skipWhitespace
    else return output

newline :: SimpleParser [String]
newline = do
  (look, _, output) <- get
  if look == '\n'
    then return output
    else expected "Newline"
