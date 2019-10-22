--https://mmhaskell.com/liftoff-3
-- creating types
module MyData where
import Data.Char (toUpper)
-- note types start with a Capital Letter in Haskell

-- BasicTask is specified as the constructor of a Task Type, it is followed by other types
-- it enables the to build objects with set type.
--it is of the format
-- data Newtype = TypeConstructor Type1 Type2 Type3

--data Task =
--    BasicTask String Int |
--    ComplexTask String Int Location

--complexTask :: Task
--complexTask = ComplexTask "Write Memo" 30 Office
----lets create a few objects
--task1 :: Task
--task1 = BasicTask "Do Assignment 1" 60

--task2 :: Task
--task2  = BasicTask "Do Laundry" 45

-- we can create a new type with multiple constructors seperated by "|"
data Location = School | Office  | Home

-- in this case the constructor is just a placeholder with not constructor details

schoolLocation :: Location
schoolLocation = School

officeLocation :: Location
officeLocation = Office

homeLocation :: Location
homeLocation = Home

-- this is cool as we can wrap different data depending on the constructor that we use. Other languages struggle to provide this flexibility.

-- Up to Parameterised Types

-- type parameters, describe the fields a user can pick for a type

data TaskLength =
    QuarterHour |
    HalfHour    |
    ThreeQuarterHour |
    Hour |
    HourAndHalf |
    TwoHours |
    ThreeHours

-- We can now add parameterisation to the Task Type, they type a is now a mystery type that we can fill
-- we will have to comment out the previous definition of Task

data Task a =
    BasicTask String a |
    ComplexTask String a Location


-- now we can add new type descriptions

complexTask :: Task TaskLength
complexTask = ComplexTask "Write Memo" HalfHour Office
--lets create a few objects
task1 :: Task Int
task1 = BasicTask "Do Assignment 1" 60

task1Different :: Task TaskLength
task1Different = BasicTask "Do Assignment 1" Hour

task2 :: Task Int
task2  = BasicTask "Do Laundry" 45

-- this add greater granularity to the program control however, now that the objects are of different types it restricts the operations that can be performed on them together .

--list example
-- at the source level, lists are constructed by two constructors Nil and Cons
data List a =
    Nil |
    Cons a (List a)
-- as expected the list type has a single paramater.
-- the Nil constructor is an empty object, so [] is using Nil
-- the cons constructor concatenates a single object with another list .
emptyList :: [Int]
emptyList = [] -- Actually Nil

fullList :: [Int]
-- Equivalent to Cons 1 (Cons 2 (Cons 3 Nil))
-- More commonly written as [1,2,3]
fullList = 1 : 2 : 3 : []

--I am not sure if we can construct our own list types, I have tried but it didn't work
--data TestList a =
--  Nil |
--  Cons a (TestList a)

--test :: TestList Int
--test = 1 : 2 : 3 : []

-- did some investigation with cons lists
-- also checked stack exchange for the error, it was interesting, check again as course progresses
--trying to define a new constructor :. , I need to check on this progress
--(:.) :: a -> List a -> List a

--testing creating a new constructor
--data (:.) a =
--    Nil |
--    a :. [a]

--testList :: [Int]
--testList = 1 :. 2 :. 3 :. []
--I am up to RECORD SYNTAX



-- 16/7/19 - Reviewing Record Syntax
-- note need to read the instructions carefull as the examples rely on the unparameterised data types (BasicTask)
-- the functions where looking for another argument (potentially a parameter of the type
-- potentially go back and debug this after the exercise.

-- we will define a new unparameterised task for ease (keep the complex Task in the example)
data Task_unparam = BasicTask_u String Int deriving (Show)
-- we cannot  keep the BasicTask constructor as it
simpleTask1 :: Task_unparam
simpleTask1 = BasicTask_u "Do Assignment 1" 60

simpleTask2 :: Task_unparam
simpleTask2 = BasicTask_u "Do Laundry" 45

simpleTask3 :: Task_unparam
simpleTask3 = tripleTaskLength simpleTask1

-- this is a function called to create a variable,
-- not sure if this is usefule as the type def is only a string
simpleTasknameupper :: String
simpleTasknameupper = capitalizedName simpleTask1

--Up to record syntax


twiceLength :: Task_unparam -> Int
twiceLength (BasicTask_u name time) = 2 * time

capitalizedName :: Task_unparam -> String
capitalizedName (BasicTask_u name time) = map toUpper name

tripleTaskLength :: Task_unparam  -> Task_unparam
tripleTaskLength (BasicTask_u name time) = BasicTask_u name (3 *time)



--finished today
--this code is working, need to createa a function to print the value(s) of Task_unparam
-- will need to define new functions (type signatures) to do this.
--https://stackoverflow.com/questions/22624924/how-do-i-print-the-name-and-value-of-a-custom-data-type-in-haskell
--showTask :: Task_unparam -> String
--showTask = show

--7/8/19 Continuing on the tutorial working from Home
-- if we add deriving (show) at the end of a data declaration it makes it automatically part of the show typeclass
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses

-- resolved why the functions weren't printing need to consider type classes, i was trying to cast
-- capitalizedName into a Basictask_u when it is infact a function.

--16/8/19 - RECORD SYNTAX
--we can create a named parameters so
data Task_named = BasicTask_n
    { taskName :: String,
    taskLength :: Int }
    deriving (Show)

simpleTask1_n :: Task_named
simpleTask1_n = BasicTask_n
    { taskName = "Do Assignment 1",
      taskLength = 60}

simpleTask2_n :: Task_named
simpleTask2_n = BasicTask_n
    { taskName = "Do Laundry",
      taskLength = 45}

tripleTask_length_n :: Task_named -> Task_named
tripleTask_length_n task = task {taskLength = 3 * (taskLength task)}
-- in this case task is just a dummy variable for pattern matching
-- this is a way to mutate data via functions.
simpleTask3_n :: Task_named
simpleTask3_n = tripleTask_length_n simpleTask1_n

--next section, the Type Keyword

--the type keyword creates a synonym for a type however it may be confusing
-- as it is syntactic sugar using the base types and another programmer may expect it to be its own type.

--type Task = (String, Int)

--twiceTaskLength :: Task -> Int
-- “snd task” is confusing here
--twiceTaskLength task = 2 * (snd task)

--"snd" is confusting as it s a function that actos on a tuple, and in the code it may not be clear that the Task type is in fact just a tuple


--New Types
-- you can create a new type which has halfway between an ADT (data constructor) and a Type

-- it is created as a new type but inherits the features of the old type, and can on ly be used on a single constructor
-- this will enable it to have all the optimisations of the original type at Compile time
--newtype TaskLength = TaskLength Int
-- is faster than
--data TaskLength = TaskLength Int

--we can used record syntax to unwrap these values based on a name, without having to do pattern matching.

--new types are also useful when wrapping existing types so that they can be debugged more clearly, rather than an error with Int, you will have an error with myCustomType.

--the convention is to use un-myCustomType to unwrap them into normal types

--data Task = BasicTask String TaskLength

--newtype TaskLength = TaskLength
--  { unTaskLength :: Int }

--this is tedious to wrap and unwrap the value but is very useful for debugging whilst still retaining the optimised abstraction and type integrity.
