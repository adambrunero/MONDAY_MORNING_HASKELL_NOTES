
--part 1 - Functors
--https://mmhaskell.com/monads/functors

tupleFromInputString :: String -> Maybe (String, String, Int)
tupleFromInputString input = if length stringComponents /= 3
  then Nothing
  else Just (stringComponents !! 0, stringComponents !! 1, age)
  where
    stringComponents = words input
    age = (read (stringComponents !! 2) :: Int)
--data type for a person, note that the constructor is the same as the data type
-- this is odd, I am not sure whether this is good practice
-- but will go with it.

data Person = Person {
    firstName :: String,
    lastName :: String,
    age :: Int}
    deriving (Show)

--function to create a person from a Tuple

personFromTuple :: (String, String, Int) -> Person
personFromTuple (fName, lName, age) = Person fName lName age

convertTuple :: Maybe (String, String, Int) -> Maybe Person
convertTuple Nothing = Nothing
convertTuple (Just t)  = Just (personFromTuple t)


--A change of format
--'sometimes we don't care how values are wrapped.
--We just want to Transform whatever underlying value exists and return the new value in the same wrapper.

--introduction to Functors
-- a functor is a typeclass in haskell
    -- for data to be an instance of the Functor typeclass it must implement a single function fmap

-- for lists, fmap is defined as the map function
-- fmap is actually a generalisation of mapping.
-- maybe is a Functor
    -- and is very simlar to the convertTuple function.
--    instance Functor Maybe where
--      fmap _ Nothing = Nothing
--      fmap f (Just a) = Just (f a)

-- if we dont have a value, return null, if we do, just apply the function and wrap it in just

--we can rewrite convertTuple, so that it is made a more generic version

convertTuple_fmap :: Functor f => f (String, String, Int) -> f Person
convertTuple_fmap = fmap personFromTuple

--testing code
--k = tupleFromInputString "Adam Brunero 1"
--l = convertTuple_fmap k
-- => Just (Person {firstName = "Adam", lastName = "Brunero", age = 1})

--as it is a functor, there is no need to deal with the Nothing cases as these are dealt with
-- by the Functor type class.

--MAKING OUR OWN FUNCTORS

--using the example

data GovDirectory a  = GovDirectory {
    mayor :: a,
    interimMayor :: Maybe a,
--    cabinet :: Map String a,
    councilMembers :: [a]}
    deriving (Show)

-- we can define the following functor instanace for GovDirectory by definging fmap
--need to review what the instance keyword does?
    -- is this just an instance of representation of a dataype?

instance Functor GovDirectory where
    fmap f oldDirectory = GovDirectory {
        mayor = f (mayor oldDirectory),
        interimMayor = f <$> interimMayor oldDirectory,
--        cabinet = f <$> cabinet oldDirectory,
        councilMembers = f <$> councilMembers oldDirectory
    }

--Note <$> is simply a synonym for fmap

-- there are a few problems with this  part of the exercise
--it could do with a review on fresh eyes
    -- Map is not a keyword
    -- convertTuple <$> oldDirectory doesnt match with typee
        -- this just a example
    -- also review => , <$>

-- 14/9/19 - I have reviewed, I think it is more the complexityy of the GovDirectory Type
-- i have reviewed other examples and have determined that this exercise is to define fmap for the
-- data type by creating a functor instance on the datatype so that It can transform itself for the
--multiple forms it is required.


--Applicative Functors (Monads Part 2)
--14/9/19
--https://mmhaskell.com/monads/applicatives

f = (*) <$> (Just 4)
--note this didn't need let as it was not run in the interpreter
--the let error gave some issue about missing parameters, a trap for young playaz

-- this is a partial function wrapped in a maybe.
-- we cannot unwrap it an apply it to Just 5 so we have to createa a function specific for maybe
-- this is painful as it only works for Maybe types reduces the generalisation of fmap.

funcMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
funcMaybe Nothing _ = Nothing
funcMaybe (Just f) val = f <$> val


--Applicatives to the Rescue.
--applicatives are a way to wrap functions in a way that is appropriate for their arguments
-- we can wrap any function then apply it sequentially rather than using an fmap
--in this case (4 *) <$> (Just 5)
-- >> Just 20
--or
--pure (4 *) <*> (Just 5)
-- >> Just 20

-- applicatives can be applied on lists
--instance Applicative [] where
--  pure a = [a]
--  fs <*> xs = [f x | f <- fs, x <- xs]

-- in this case the function in the first list (fs) is applied to each element in the second (xs)

-- this makes it an easy way to find the pairwise product of two lists.
-- >> pure (*) <*> [1,2,3] <*> [10,20,30]
-- [10,20,30,20,40,60,30,60,90]


--Part 3 - !!!!MONADS!!!!
--https://mmhaskell.com/monads/tutorial
-- what a build up
-- a monad wraps a value or computation in a particular context
-- a monad must define both
--  1. a means of wrapping normal values in the context, and
--  2. a way of combining computations in the context

-- a monad is defined in a type class, it has two fuctions
-- class Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> a -> m b -> m b

-- the return function specifies how to wrap values in the Monad's context
--      a -> m a
-- the >>= operator (called bind), specifies how to combine two operations within the context

--The Maybe Monad
maybeFunc1 :: String -> Maybe Int
maybeFunc1 "" = Nothing
maybeFunc1 str = Just $ length str

-- note $ is the infix operator
--https://stackoverflow.com/questions/19521246/what-does-mean-do-in-haskell
--($) :: (a -> b) -> (a -> b)
--f $ x = f x
-- or
--($) f x = f x
-- or
--($) = id
--It's useful for avoiding extra parentheses: f (g x) == f $ g x

maybeFunc2 :: Int -> Maybe Float
maybeFunc2 i = if i `mod` 2 == 0 --note that these are backticks `` not apostrophes ''
    then Nothing
    else Just ((fromIntegral i ) * 3.14159)

maybeFunc3 :: Float -> Maybe [Int]
maybeFunc3 f = if f > 15.0
    then Nothing
    else ($) Just [floor f, ceiling f]
-- this is an error in the notes, else $ Just [floor f, ceiling f]
-- this is redundant,  just needs brackets as else is not a function!

runMaybeFuncs :: String -> Maybe [Int]
runMaybeFuncs input = case maybeFunc1 input of
    Nothing -> Nothing
    Just i -> case maybeFunc2 i of
        Nothing -> Nothing
        Just f -> maybeFunc3 f

-- this is a triangular nesting patter which is pretty much the same thing
-- if there are more functions, there will be more maybes and it will keep getting worse
-- an harder to determine control flow.

--THis is how haskell implements the maybe Monads,
--instance Monad Maybe where
--    return Just     -- this is the means to wrap the values in the context (1)
--    Nothing >>= _ = Nothing -- a way of combining computations in the context .
--    Just a >>= f = f a
-- activity rewrite all of this as an Adam_Maybe

runMaybeFuncsMonad :: String -> Maybe[Int]
runMaybeFuncsMonad input = maybeFunc1 input >>= maybeFunc2 >>= maybeFunc3

-- a good place to stop these are working
-- remember the backticks! ` ` ` ` `
--helpful primer on monads
--https://bartoszmilewski.com/2016/12/27/monads-categorically/


-- the IO Monad, just some detail on how side effects are treated in the overall structure of a program

-- eg the whole program is a monad, as it uses pure functions within a context and then
-- refers them back to the output
