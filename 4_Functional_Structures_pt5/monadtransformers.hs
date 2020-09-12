-- main :: IO
-- main = do
--   maybeUserName <- readUserName
--   case maybeUserName of
--     Nothing -> print “Invalid user name!”
--     Just (uName) -> do
--       maybeEmail <- readEmail
--       case maybeEmail of
--         Nothing -> print “Invalid email!”
--         Just (email) -> do
--           maybePassword <- readPassword
--           Case maybePassword of
--             Nothing -> print “Invalid Password”
--             Just password -> login uName email password
--
-- readUserName :: IO (Maybe String)
-- readUserName = do
--   str <- getLIne
--   if length str > 5
--     then return $ Just str
--     else return Nothing
--
-- readEmail :: IO (Maybe String)
-- ...
--
-- readPassword :: IO (Maybe String)
-- ...
--
-- login :: String -> String -> String -> IO ()

--the program above has lots of nesting and calls to IO which is not easy to debug

---------------------------things to get code working
-- just imported the pre-made version of MaybeT

import Control.Monad
import Control.Monad.Trans (lift)
-- import Control.Monad.Trans.Maybe (runMaybeT)
-- import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Maybe

--only needed to import the higher Maybe and not the types or function
    -- why didnt this work
-- originally lift did not work even when imported
  -- need to review the role of lift

--    https://stackoverflow.com/questions/28495819/types-in-maybet-computation

-- https://stackoverflow.com/questions/32579133/simplest-non-trivial-monad-transformer-example-for-dummies-iomaybe


-- *also why did declaring the data type not work, said something about needing an applicatiive
--
-- * Could not deduce (Applicative (MaybeT m))
--         arising from the superclasses of an instance declaration
--       from the context: Monad m
--         bound by the instance declaration at monadtransformers.hs:65:10-38
--     * In the instance declaration for `Monad (MaybeT m)'
--https://wiki.haskell.org/New_monads/MaybeT


-----------------------------------------------------------------
-- we can simplify this by using MaybeT to wrap the IO MONAD
-- our new Monad is MaybeT IO
-- the functions are

--
-- newtype MaybeT m a  = MaybeT { runMaybeT :: m (Maybe a)}
--
--
-- instance (Monad m) => Monad (MaybeT m) where
--   return = lift . return
--   x >>= f = MaybeT $ do
--     v <- runMaybeT x
--     case v of -- this was a misaligned block
--       Nothing -> return Nothing
--       Just y -> runMaybeT (f y)

readUserName :: MaybeT IO String
readUserName = MaybeT $ do
    str <- getLine
    if length str > 5
      then return $ Just str
      else return Nothing

readEmail :: MaybeT IO String
readEmail = MaybeT $ do
    str <- getLine
    if length str > 10  -- just using a length based criteria for testing purposes.
      then return $ Just str
      else return Nothing

readPassword :: MaybeT IO String
readPassword = MaybeT $ do
  str <- getLine
  if length str < 10
    then return Nothing
    else return $ Just str

-- now we can wrap all three of these calls into a single monadic action and do a single pattern match to get the results
-- we will use the runMaybeT function to unwrap the Maybe value from MaybeT

main :: IO ()
main = do
  maybeCreds <- runMaybeT $ do
    usr <- readUserName
    email <- readEmail
    pass <- readPassword
    return (usr, email, pass)
  case maybeCreds of
    Nothing -> print "Couldn't Login!"
    Just (u, e, p) -> print "login details are Correct, that is all "
