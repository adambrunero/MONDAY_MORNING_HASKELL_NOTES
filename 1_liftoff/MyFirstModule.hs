--when loading a module use :l ModuleName to load it in GHCi

module MyFirstModule where
myFirstExpression :: String
myFirstExpression = "Hello World"

-- if you down write the type of a function it will be inferred.
myFirstFunction :: String -> String
myFirstFunction input = "Hello " ++ input ++ " !"


power_ab :: Float -> Int -> Float
power_ab x 0 = 1.0
power_ab x n = x * (power_ab x (n-1))

myIfStatement :: Int -> Int
myIfStatement a = if a <= 2
    then a + 2
    else a - 2

--this can be rewritted with a guard with more concise code, else -> otherwise, for each case of the statement, the "|" symbolis required otherwise you will get a parse error

myGuardStatement :: Int -> Int
myGuardStatement a
    | a <= 2 = a + 2
    | a <= 6 = a
    | otherwise = a - 2


--PATTERN MATCHING
-- you can change the behaviour of your code based on the structure of an object

myFunction :: [Int] -> Int
myFunction [a] = a + 3
myFunction [a, b] = a + b + 1
myFunction (1 : 2: _) = 3
myFunction (3 : 4: _) = 7
myFunction xs = length xs

--not the order of the pattern evaluation is procedural, so if a catchall statement is first, It will always be run, however the compiler will warn for this.
-- the underscore value works as a wildcard value and allows matching on any value


--UP to case statemetns.

-- CASE STATEMENTS
-- the above function can be written as a case statement.
myFunctioncase :: Bool -> [Int] -> Int
myFunctioncase usePattern xs = if not usePattern
    then length xs
    else case xs of
        [a] -> a + 3
        [a, b] -> a + b + 1
        (1 : 2 : _) -> 3
        (3 : 4 : _) -> 7
        _ -> 1

-- this uses the arrows rather than assignment, I am not sure why, I will leave this to answer later.

-- WHERE AND LET
-- Haskell doesn't really have variables as expressions don't change their value
-- you can define sub expressions this way

mathFunction :: Int -> Int -> Int -> Int
--mathFunction a b c  = (c - a) + (b - a) + (a * b * c) + a
-- this is not so readable
mathFunction a b c  = diff1 + diff2 + prod + a
    where
    diff1 = c - a
    diff2 = b - a
    prod = a * b * c
-- these variables can be defined within the function, care needs to be taken to ensure there isn't
-- a dependency on the variables which will cause loops.

-- you can use a let function with an in, the expression is detailed after the in


mathFunctionLet :: Int -> Int -> Int -> Int
mathFunctionLet a b c =
    let
    diff1 = c - a
    diff2 = b - a
    prod = a * b * c
    in diff1 + diff2 + prod + a

-- you can declare functions within functions, this si useful for IO
--main :: IO ()
--main = do
--  input <- getLine
--  print (repeatFunction input)
--  where
--    repeatFunction xs = replicate 3 xs

--check out this project
--https://academy.mondaymorninghaskell.com/p/your-first-haskell-project

-- UP TO SECTION 3
-- Part 3  - Haskell Types
