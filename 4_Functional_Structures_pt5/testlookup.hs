--example of lookup from the Haskell notes.

import Prelude hiding (lookup)
import Data.Map


-- notes fromList returns a container with all the elements from the List in key, value pairs

-- lookup returns the value from a key and a  key value pair
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
employeeDept = fromList([("John","Sales"), ("Bob","IT")])
deptCountry = fromList([("IT","USA"), ("Sales","France")])
countryCurrency = fromList([("USA", "Dollar"), ("France", "Euro")])

employeeCurrency :: String -> Maybe String
employeeCurrency name = do
    dept <- lookup name employeeDept
    country <- lookup dept deptCountry
    lookup country countryCurrency

main = do
    putStrLn $ "John's currency: " ++ (show (employeeCurrency "John"))
