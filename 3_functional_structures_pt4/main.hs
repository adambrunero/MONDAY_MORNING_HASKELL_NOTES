-- Reader and Writer Monads
-- https://mmhaskell.com/monads/reader-writer

-- GLOBAL VARIABLES (OR A LACK THEREOF)
-- in haskell the code is generally "pure" as such, functions can only interact with the arguments passed to them
-- this means we can't have global variables.
-- we can have global expressions, however these are fixed at compile time.

import Control.Monad.Reader

main :: IO ()
main  = do
    env <- loadEnv
    let str = runReader func1 env
    print str

data Environment = Environment
    { param1 :: String
    , param2 :: String
    , param3 :: String } deriving (Show)

loadEnv :: IO Environment
loadEnv = do
    test <- getLine
    let input_env = Environment {param1 = test ++ "1", param2 = test ++ "2", param3 = test ++ "3"}
    return input_env

func1 :: Reader Environment String
func1 = do
    res <- func2
    return ("Result: " ++ (show res))

func2 :: Reader Environment Int
func2 = do
    env <- ask
    let res3 = func3 env
    return (2 + (floor res3))

func3 :: Environment -> Float
--func3 (Environment param1 param2 param3) = (fromIntegral (length param1))*3.14
-- this can be re-written more neater due to named parameters
func3 test = (fromIntegral (length (param1 test)))*3.14
-- in the line above test is just a dummy variable to access the parameters of the Environment Object


-- The READER SOLUTION
-- the reader MONAD means that we don't have to pass the environment through all function calls
-- even if they don't use it.
-- the Reader monad creates a globa read-only value of a specified type.
    -- all functions within the Monad can read the object


-- with the Reader monad, all functions that area within the Monad will have access to the environment
-- the environment has been loaded through loadEnv which is then passed to the funcs by runReader

-- the ask function unwraps the environment so we can use it
-- the monads bind action allows us to glue the reader functions together
-- all we need to do is runReader and supply the environment as a parameter
    -- all functions within the action will be able to use the environment as a global variable


-- interesting things to get this to work
        -- remember to cast to float using fromIntegral
        -- as the data type is parameterised it can be unwrapped from dummy variables in functions
        -- loading an named record required the parenthese listed blow
x :: Environment
x = Environment
    {   param1 = "test1",
        param2 = "test2",
        param3 = "test3"}

test_func :: Environment -> Int
test_func test = length (param1 test)
