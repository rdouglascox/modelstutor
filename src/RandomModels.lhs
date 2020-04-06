> module RandomModels where

> import System.Random
> import Wffs
> import Enumerator
> import GPLIparser
> import GPLIevaluator
> import Data.List

---randomchoice function

> randomchoice g xs = getvalue randominteger $ enumerate xs
>    where randominteger = fst (randomR (1, length xs) g)

> randomchoices g x xs = getvalues randominteger $ enumerate xs
>     where randominteger = take x (randomRs(1,(length xs)) g)

> enumerate :: [a] -> [(Int,a)]
> enumerate xs = zip [1..(length xs)] xs

> getvalue :: Int -> [(Int,a)] -> a
> getvalue x (y:ys) = if fst y == x
>                     then snd y
>                     else getvalue x ys

> getvalues xs ys = [y | (z,y) <- ys, z `elem` xs]

-- these two are the main random functions we use. the first returns a random item from a list, the other returns n random items from a list

> getrandom :: [a] -> IO a
> getrandom xs = do
>                newStdGen
>                gen <- getStdGen
>                let out = randomchoice gen xs
>                return out 

> getrandoms :: Int -> [a] -> IO [a]
> getrandoms x xs = do
>                   newStdGen
>                   gen <- getStdGen
>                   let out = randomchoices gen x xs
>                   return out 

> randomgetrandoms :: [a] -> IO [a]
> randomgetrandoms xs = do
>                         num <- getrandom [1..(length xs)]
>                         newStdGen
>                         gen <- getStdGen
>                         let out = randomchoices gen num xs
>                         return out 

> randomgetrandompairs :: [a] -> IO [(a,a)]
> randomgetrandompairs xs = do
>                           num <- getrandom [1..(length xs)]
>                           newStdGen
>                           gen <- getStdGen
>                           let left = randomchoices gen num xs
>                           newStdGen
>                           gen <- getStdGen
>                           let right = randomchoices gen num xs
>                           return (zip left right) 



Okay, we need to do better for random models than just to enumerate them all! There are just too many of them. 

First we get a random domain.

> getdomain :: IO [Int]
> getdomain = getrandom [[1],[1,2],[1,2,3],[1,2,3,4]]

Once we've got a domain, an input string and have parsed it, we get an assignment of referents for the names.

> myparser x = head (parser x)

> getreferents xs ys = do
>                      refs <- sequence [getrandom ys | x <- (getnames xs)] 
>                      return (zip (getnames xs) refs) 

> getoneplace xs ys = do
>                     refs <- sequence [randomgetrandoms  ys | x <- (getpredicates1 (myparser xs))] 
>                     return (zip (getpredicates1 (myparser xs)) (refs ++ (replicate 50 []))) 

Cool. Now we do similar things for pairs and triples. Shit! I can't quite see how this might be done.

> gettwoplace xs ys = do
>                     refs <- sequence [randomgetrandoms (allPairs ys) | x <- (getpredicates2 (myparser xs))] 
>                     return (zip (delete 'I' (getpredicates2 (myparser xs))) (refs ++ (replicate 50 []))) 

> getthreeplace xs ys = do
>                     refs <- sequence [randomgetrandoms (allTriples ys) | x <- (getpredicates3 (myparser xs))] 
>                     return (zip (getpredicates3 (myparser xs)) (refs ++ (replicate 50 []))) 


> getrandommodel :: [Char] -> IO Model
> getrandommodel x = do
>                    dom <- getdomain   
>                    refs <- getreferents x dom
>                    ones <- getoneplace x dom
>                    twos <- gettwoplace x dom
>                    thrs <- getthreeplace x dom
>                    return (Model dom refs ones twos thrs)  
