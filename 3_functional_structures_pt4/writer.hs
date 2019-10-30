--example of a writer monad, edited to increment int
--https://mmhaskell.com/monads/reader-writer

--Up to Accumulating Values.

-- The accumulation problem, how do we record the computational cost  of running  multiple functions
-- we can do this by using accumulator arguments to keep track of the cost so far and passing the value along.

-- the generalisation of this typeclass is the Monoid

-- the int is a simple example of a Monoid, defined as
-- class Monoid a where
--  mempty :: a
--  mappend :: a -> a -> a

--This is an accumulation class,. it defines two functions mempty is the initial value of the monoid
-- then mappend is a function that takes two of these values into a result.

-- top make a monoid instance for int, the description would be

--  instance Monoid Int where
--      mempty = 0
--      mappend a b = a + b
-- our accumulator starts at 0 and we combing values by adding them.

-- the writer value is an accumulator, we can change the verbose functions in a similar vein by
-- noting that they exist in a context of global values

-- the functions are as such

import Control.Monad.Writer
import Data.Monoid

-- a new type as Int has no monoid associated with it
type Total  = Sum Int

wfunc1 :: String -> (String, Total)
wfunc1 input  = if length input `mod` 2 == 0
    then runWriter (wfunc2 input)
    else runWriter $ do
        str1 <- wfunc3 input
        str2 <- wfunc4 (take 1 input)
        return (str1 ++ str2)

wfunc2 :: String -> Writer Total String
wfunc2 input = if (length input > 10)
    then do
        tell 1
        wfunc4 (take 9 input)
    else do
        tell 10
        return input

wfunc3 :: String -> Writer Total String
wfunc3 input = if (length input) `mod` 3 == 0
    then do
        tell 3
        wfunc2 (input ++ "ab")
    else do
        tell 1
        return $ tail input

-- remember that $ is the infix operator, a good way to eliminate brackets

wfunc4 :: String -> Writer Total String
wfunc4 input = if (length input) < 10
    then do
        --getSum tell $ wfunctinttosum $ (length input)
        tell $ Sum (length input)
        -- (length Input Creates an Int, needs to be converted to a sum int to be added to total
        return (input ++ input)
    else do
        tell 5
        return (take 5 input)

--need to review monoid instances for Int
--https://stackoverflow.com/questions/29499119/why-int-does-not-implement-monoid

--wfuncinttosum :: Int -> Sum
--wfuncinttosum input  = Sum input

-- a new approach create a custom into type witha  sum monad

--newtype AddInt = AddInt Int

--instance Monoid AddInt where
--    mempty = 0
--    mappend a b = a + b

-- the above didn't woork
--created a new type Total that was a Sum Int
-- so that int could be incremented as part of the sum newtype
--used this a tutorial, review for an exercise
--http://learnyouahaskell.com/for-a-few-monads-more
