> module Main where

> import Lib
> import System.Console.ANSI
> import System.Random
> import System.IO
> import Wffs
> import Enumerator
> import GPLIparser
> import GPLIevaluator
> import RandomModels
> import System.Exit
> import PrintModels

> main :: IO ()
> main = do 
>        hSetBuffering stdout NoBuffering
>        wff <- wffs
>        putStrLn ""
>        putStrLn ("formula: " ++ wff)
>        mod <- (getrandommodel wff)
>        putStrLn (printmodel mod)
>        let val = (newval wff mod)
>        putStr "value: "
>        ans <- checkinput
>        setSGR [SetColor Foreground Vivid Magenta]
>        if (read ans :: Bool) == val
>            then do
>                 putStrLn "\nCorrect!\n"
>            else do
>                 putStrLn "\nIncorrect!\n"  
>        setSGR [Reset]
>        continue


> checkinput = do
>              input <- getLine
>              if input == ":q" 
>              then do 
>                   exitSuccess
>                   else do
>                        if input == "True" || input == "False"
>                        then do return input
>                        else do 
>                             putStr "Please type either True or False (or :q to quit): " 
>                             checkinput  


> continue = do
>            putStr "Press Enter to continue or :q to quit. "
>            input <- getLine
>            if input == ":q"
>            then do
>                 exitSuccess
>            else do 
>                 main  
