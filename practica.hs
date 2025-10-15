import Parsing
import Control.Monad
import Control.Applicative hiding (many)
import Data.Char

transformer :: Parser a -> Parser a 
transformer p = do 
        char '('
        x <- p 
        char ')'
        return x
    <|>
        p

withQuotes :: Parser a -> Parser a
withQuotes p =
    do
        char '"'
        x <- p
        char '"'
        return x
    <|>
    do
        char '\''
        x <- p
        char '\''
        return x
    <|>
    p

sepList :: Parser a -> [Parser sep] -> Parser [a]
sepList p [] = return []
sepList p (x:xs) = sepBy p x <|> sepList p xs

sepList2 :: Parser a -> [Parser sep] -> Parser [a]
sepList2 p seps = sepBy p (foldr1 (<|>) seps)

data Tuple = Single Int | Pair Tuple Tuple deriving (Show)

tuple :: Parser Tuple
tuple =
    do 
        n <- int
        return (Single n)
    <|>
    do  
        char '('
        x <- tuple
        char ','
        y <- tuple
        char ')'
        return (Pair x y)

data Interval = Interval {leftOpen :: Bool
                         , rightOpen :: Bool
                         , a :: Float
                         , b :: Float } deriving Show

interval :: Parser Interval
interval = 
    do
        sep1 <- char '(' <|> char '['
        a <- float'
        char ','
        b <- float'
        sep2 <- char ')' <|> char ']'
        let left = if sep1 == '(' then True else False
        let right = if sep2 == ')' then True else False
        return (Interval left right a b)

float :: Parser Float
float = 
    do
        s <- sign
        xs <- many1 digit       
        char '.' -- ¿Que sucede cuando no encuentra el '.'?
        ys <- many digit 
        let ys' = case ys of
                    [] -> "0"
                    _  -> ys
        let value = read (xs ++ "." ++ ys')
        return (s * value)

sign :: Parser Float
sign = (char '-' >> return (-1)) <|> return 1
-- Esto quiere decir que si encuentra un '-' lo ignora y 
-- devuelve -1, si el parser falla (es decir no lo encontro)
-- entonces devuelve 1

-- Funcion de la bibliografia:
float' :: Parser Float
float' =
  do sign <- (char '-' >> return (-1))
                 <|> (char '+' >> return 1)
                 <|> return 1
     xs <- some digit
     frac <- (do char '.'
                 ys <- some digit
                 return (read ("0." ++ ys)))
             <|> return 0
     return $ sign * (read xs + frac)

-- El simbolo $ evita el uso de parentesis f (g x)

linspace :: Parser [Float]
linspace = do
            char '('
            inicio <- float' 
            char ','
            fin <- float'
            char ','
            n <- int
            char ')'
            let salto = (fin - inicio) / fromIntegral (n - 1)
            return (makeList inicio n salto)

makeList :: Float -> Int -> Float -> [Float]
makeList inicio 1 _ = [inicio]
makeList inicio n salto = inicio : makeList (inicio + salto) (n-1) salto

data Multi = MultiInt Int | MultiChar Char deriving Show

listaHeterogenea:: Parser [Multi]
listaHeterogenea = 
    do
        char '['
        xs <- sepBy toMulti (char ',')
        char ']'
        return (xs)

toMulti :: Parser Multi
toMulti =
    do
        value <- int
        return (MultiInt value)
    <|>
    do
        char '\''
        value <- alphanum
        char '\''
        return (MultiChar value)

data Expr = Num Int | BinOp Op Expr Expr deriving Show
data Op = Add | Mul | Res | Div deriving Show

expr :: Parser Expr
expr = do
    t <- term
    (do char '+'
        e <- expr
        return (BinOp Add t e)
     <|>
     do char '-'
        e <- expr
        return (BinOp Res t e)
     <|>
     return t)


term :: Parser Expr
term = do
    f <- factor
    (do char '*'
        t <- term
        return (BinOp Mul f t)
     <|>
     do char '/'
        t <- term
        return (BinOp Div f t)
     <|>
     return f)


factor :: Parser Expr
factor = 
    do
        d <- int
        return (Num d)
    <|>
    do
        char '('
        e <- expr
        char ')'
        return e