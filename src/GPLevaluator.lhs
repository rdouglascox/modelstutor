> module GPLevaluator (val, genassignment, parser, Model (..)) where

> import GPLparser

PROGRAM FUNCTIONS
=================

Okay, so here's our recursive definition of Propositions in GPL. This is what we want our parser to give us. 

< data Prop = Atomic Predicate [Char]
<           | Negation Prop
<           | Conjunction Prop Prop
<           | Disjunction Prop Prop
<           | Conditional Prop Prop
<           | Biconditional Prop Prop
<           | Existential Char Prop
<           | Universal Char Prop
<           deriving (Show)

We distinguish between one, two, and three place predicates.

< data Predicate = Predicate' Char 
<                | Predicate'' Char
<                | Predicate''' Char
<                deriving (Show)

We just import the data types here from GPLparser.

We use Haskell's "Record Syntax" to define out data type for Models.

> data Model = Model {domain :: [Int]
>                    ,referents :: [(Char, Int)]
>                    ,extensions' :: [(Char, [Int])]
>                    ,extensions'' :: [(Char, [(Int,Int)])]
>                    ,extensions''' :: [(Char, [(Int,Int,Int)])]
>                    }
>                    deriving (Show)

Okay, the nice thing about the "Record Syntax" definition of our type for models is that qwe get functions from models to particular values for free. So `domain mymodel` will evaluate to `[1,2,3,4,5]`. 

Okay, the values we get back from model functions are lists of characters and pairs. They key value pairs. So it would be good to have a general function for getting the value of some key. Let's call it `find`:

> find k t = head [ v | (k', v) <- t, k == k'] 

Okay, cool, that will be handy. Now we need to write a function for interpreting terms. So lets assume with have a variable assignment like this:

Now, here's our function for terms.

> iterms :: Model -> Assignment -> [Char] -> [Int]
> iterms m g xs = map f xs
>     where f x = if x `elem` ['a'..'t']
>                 then find x (referents m)
>                 else find x g      

Okay, now we need a function for predicates.

> ipreds' :: Model -> Char -> [Int]
> ipreds' m x = find x (extensions' m)  

> ipreds'' :: Model -> Char -> [(Int,Int)]
> ipreds'' m x = find x (extensions'' m)  

> ipreds''' :: Model -> Char -> [(Int,Int,Int)]
> ipreds''' m x = find x (extensions''' m)  


And now we want to define the valuation function.

> val :: Model -> Assignment -> Prop -> Bool
> val m g (Atomic (Predicate' x) y) = (tosing (iterms m g y)) `elem`  (ipreds' m x) 
> val m g (Atomic (Predicate'' x) y) = (topair (iterms m g y)) `elem` (ipreds'' m x) 
> val m g (Atomic (Predicate''' x) y) = (totriple (iterms m g y)) `elem` (ipreds''' m x)
> val m g (Negation scope) = not (val m g scope)
> val m g (Conjunction left right) = and [val m g left, val m g right] 
> val m g (Disjunction left right) =  or [val m g left, val m g right]
> val m g (Conditional first second) = or [not (val m g first), val m g second]
> val m g (Biconditional left right) = and [val m g (Conditional left right),val m g (Conditional right left)] 
> val m g (Existential x scope) = or (map (\z -> (val m (newa g x z) scope )) (domain m))
> val m g (Universal x scope) = and (map (\z -> (val m (newa g x z) scope )) (domain m))

We need a helper function which takes a list and puts each bit in a tuple.

> tosing (x:[]) = x
> topair (x:y:[]) = (x,y)
> totriple (x:y:z:[]) = (x,y,z) 


We need a reassignment function. The following type should be read something like this: takes an assignment and replaces the assignment of 'x' so that it maps to 1 in the new assignment. 

> newa :: Assignment -> Char -> Int -> Assignment
> newa g c i = [(c,i)] ++ [ x | x <- g, (fst x) /= c]   

Cool, so what we'll want is to specify the evluation function for each of the above. Something like this. (We will need to relativise to a Model and Assignment eventually).

That's it!

Well, actually, we want to auto-generate our assignment for variables. At this stage, we will just ask for a list of characters in use as variables, a model, and return an assignment.

> genassignment :: Model -> [Char] -> Assignment
> genassignment m c = [(x, head (domain m)) | x <- (getvars c)]
>     where  getvars xs = [ x | x <- xs, x `elem` ['u'..'z']]

 
SOME DEFINITIONS FOR TESTING
============================
  
Here's a test model to experiment with.

> mymodel :: Model
> mymodel = Model mydomain myreferents myextensions' myextensions'' myextensions'''
>     where mydomain = [1,2,3,4,5]
>           myreferents = [('a',2),('b',1)]
>           myextensions' = [('F',[1,2,3,4,5]),('H',[])]
>           myextensions'' = [('G',[(1,2),(2,1)])]
>           myextensions''' = []

First, an example assignment.

> type Assignment = [(Char, Int)]

> myassignment :: [(Char, Int)]
> myassignment = [('x',1),('y',1)]

Cool. Atomic propositions done. Here are some example propositions.

> myprop' = (Atomic (Predicate' 'H') "x")
> myprop'' = (Atomic (Predicate'' 'G') "ax")

> myprop''' = (Existential 'x' myprop')
> myprop'''' = (Universal 'x' myprop'')

> myprop''''' = (Biconditional myprop'''' (Negation myprop'''))

