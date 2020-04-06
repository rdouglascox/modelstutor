> module GPLparser (parser, Prop (..), Predicate(..), getpredicates', getpredicates'', getpredicates''', getnames, rmdups) where

> import Control.Monad (liftM, ap)
> import Data.Char
> import Data.List

-- parse function

> parser :: String -> [Prop]
> parser x = if null (apply prop x) ||not  (null (snd (head (apply prop x))))
>            then [] 
>            else [fst $ head $ apply prop x] 

-- general parser functions

> newtype Parser a = Parser (String -> [(a,String)])
> apply :: Parser a -> String -> [(a,String)]
> apply (Parser p) s = p s

> parse :: Parser a -> String -> a
> parse p = fst . head . apply p

--- type declaration

> instance Monad Parser where
>    return x = Parser (\s -> [(x,s)])
>    p >>= q  = Parser (\s -> [(y,s'')
>                            | (x,s') <- apply p s,
>                              (y,s'') <- apply (q x) s'])

> instance Functor Parser where
>    fmap = liftM

> instance Applicative Parser where
>     pure = return
>     (<*>) = ap

> getc :: Parser Char
> getc = Parser f
>        where f [] = []
>              f (c:cs) = [(c,cs)]

> sat :: (Char -> Bool) -> Parser Char
> sat p = do {c <- getc;
>            if p c then return c
>            else ffail}

> ffail = Parser (\s -> [])

> char :: Char -> Parser ()
> char x = do {c <- sat (==x); return ()}

> string :: String -> Parser ()
> string [] = return ()
> string (x:xs) = do {char x; string xs; return ()}

--- choice operator for parsers

> (<|>) :: Parser a -> Parser a -> Parser a
> p <|> q = Parser f
>           where f s = let ps = apply p s in 
>                       if null ps then apply q s
>                       else ps

-- parsers for PL

> data Prop = Atomic Predicate [Char]
>           | Negation Prop
>           | Conjunction Prop Prop
>           | Disjunction Prop Prop
>           | Conditional Prop Prop
>           | Biconditional Prop Prop
>           | Existential Char Prop
>           | Universal Char Prop
>           deriving (Show)

> data Predicate = Predicate' Char
>                | Predicate'' Char
>                | Predicate''' Char
>                deriving (Show)


> atomic' :: Parser Prop
> atomic' = do x <- preds
>              y <- term
>              return (Atomic (Predicate' x) [y])

> atomic'' :: Parser Prop
> atomic'' = do x <- preds
>               y <- term
>               z <- term
>               return (Atomic (Predicate'' x) [y,z])

> atomic''' :: Parser Prop
> atomic''' = do x <- preds
>                y <- term
>                z <- term
>                u <- term
>                return (Atomic (Predicate''' x) [y,z,u])

> preds :: Parser Char
> preds = do c <- sat (`elem` ['A'..'Z'])
>            return (c)

> term :: Parser Char
> term = do c <- sat (`elem` ['a'..'z'])
>           return (c)

> neg :: Parser Prop
> neg = do c <- sat (=='~')
>          x <- prop 
>          return (Negation x)

> conj :: Parser Prop
> conj = do x <- parens
>           y <- prop
>           z <- conj'
>           u <- prop
>           t <- parens
>           return (Conjunction y u)  

> disj :: Parser Prop
> disj = do x <- parens
>           y <- prop
>           z <- disj'
>           u <- prop
>           t <- parens
>           return (Disjunction y u)  

> cond :: Parser Prop
> cond = do x <- parens
>           y <- prop
>           z <- cond'
>           u <- prop
>           t <- parens
>           return (Conditional y u)  

> bicon :: Parser Prop
> bicon = do x <- parens
>            y <- prop
>            z <- bicon'
>            u <- prop
>            t <- parens
>            return (Biconditional y u)  

> parens :: Parser ()
> parens = do c <- sat (=='(') 
>             return () 
>         <|> do c <- sat (==')')
>                return ()

> conj' :: Parser ()
> conj' = do c <- string' "&"
>            return ()

> disj' :: Parser () 
> disj' = do c <- string'  "v"
>            return ()

> cond' :: Parser ()
> cond' = do c <- string' "->" 
>            return ()

> bicon' :: Parser ()
> bicon' = do c <- string' "<->"
>             return ()

> string' :: String -> Parser String
> string' [] = return []
> string' (x:xs) = do char x
>                     string' xs
>                     return (x:xs)

> exi :: Parser Prop
> exi = do x <- sat (== '#') 
>          c <- sat (`elem` ['u'..'z'])
>          p <- prop
>          return (Existential c p)
 
> uni :: Parser Prop
> uni = do x <- sat (== '@') 
>          c <- sat (`elem` ['u'..'z'])
>          p <- prop
>          return (Universal c p)

> iden :: Parser Prop
> iden = do x <- sat (`elem` ['a'..'z'])
>           y <- sat (=='=')
>           z <- sat (`elem` ['a'..'z'])
>           return (Atomic (Predicate'' 'I') [x,z])

> niden :: Parser Prop
> niden = do x <- sat (`elem` ['a'..'z'])
>            y <- string' "/="
>            z <- sat (`elem` ['a'..'z'])
>            return (Negation (Atomic (Predicate'' 'I') [x,z]))

> prop :: Parser Prop
> prop = do x <- atomic''' 
>           return (x)
>        <|> do x <- atomic''
>               return (x)
>        <|> do x <- atomic'
>               return (x)
>        <|> do x <- niden
>               return (x)
>        <|> do x <- iden 
>               return (x)
>        <|> do x <- conj
>               return (x)
>        <|> do x <- disj
>               return (x)         
>        <|> do x <- bicon
>               return (x)
>        <|> do x <- cond
>               return (x)
>        <|> do x <- uni
>               return (x)
>        <|> do x <- exi
>               return (x)


END PARSER

Some helper functions.

> getpredicates' :: Prop -> [Char]
> getpredicates' (Negation x) = getpredicates' x
> getpredicates' (Disjunction x y) = (getpredicates' x) ++ (getpredicates' y)
> getpredicates' (Conjunction x y) = (getpredicates' x) ++ (getpredicates' y)
> getpredicates' (Conditional x y) = (getpredicates' x) ++ (getpredicates' y)
> getpredicates' (Biconditional x y) = (getpredicates' x) ++ (getpredicates' y)
> getpredicates' (Existential x y) = (getpredicates' y)
> getpredicates' (Universal x y) = (getpredicates' y)
> getpredicates' (Atomic (Predicate' x) y) = [x]
> getpredicates' (Atomic (Predicate'' x) y) = []
> getpredicates' (Atomic (Predicate''' x) y) = []


> getpredicates'' :: Prop -> [Char]
> getpredicates'' (Negation x) = getpredicates' x
> getpredicates'' (Disjunction x y) = (getpredicates'' x) ++ (getpredicates'' y)
> getpredicates'' (Conjunction x y) = (getpredicates'' x) ++ (getpredicates'' y)
> getpredicates'' (Conditional x y) = (getpredicates'' x) ++ (getpredicates'' y)
> getpredicates'' (Biconditional x y) = (getpredicates'' x) ++ (getpredicates'' y)
> getpredicates'' (Existential x y) = (getpredicates'' y)
> getpredicates'' (Universal x y) = (getpredicates'' y)
> getpredicates'' (Atomic (Predicate'' x) y) = [x]
> getpredicates'' (Atomic (Predicate' x) y) = []
> getpredicates'' (Atomic (Predicate''' x) y) =[]


> getpredicates''' :: Prop -> [Char]
> getpredicates''' (Negation x) = getpredicates' x
> getpredicates''' (Disjunction x y) = (getpredicates''' x) ++ (getpredicates''' y)
> getpredicates''' (Conjunction x y) = (getpredicates''' x) ++ (getpredicates''' y)
> getpredicates''' (Conditional x y) = (getpredicates''' x) ++ (getpredicates''' y)
> getpredicates''' (Biconditional x y) = (getpredicates''' x) ++ (getpredicates''' y)
> getpredicates''' (Existential x y) = (getpredicates''' y)
> getpredicates''' (Universal x y) = (getpredicates''' y)
> getpredicates''' (Atomic (Predicate''' x) y) = [x]
> getpredicates''' (Atomic (Predicate' x) y) = []
> getpredicates''' (Atomic (Predicate'' x) y) = []


> getnames :: [Char] -> [Char]
> getnames xs = [ x | x <- xs, x `elem` ['a'..'t']]

> rmdups :: Ord a => [a] -> [a]
> rmdups = map head . group . sort






