-- note this code is compiled with
-- ghc -o testgov .\testgov.hs

-- this looks like a good guide: https://www.seas.upenn.edu/~cis552/12fa/lectures/stub/QuickCheck.html
import Test.QuickCheck
import Data.Map(Map) -- this is to remove the Map type constructor error on line 7

data GovDirectory a = GovDirectory {
  mayor :: a,
  interimMayor :: Maybe a,
  cabinet :: Map String a,
  councilMembers :: [a]
} deriving (Show, Eq) -- this needs to be added so that the typeclass can inherit the properties printing and Eq


instance Functor GovDirectory where
  fmap f oldDirectory = GovDirectory {
    mayor = f (mayor oldDirectory),
    --interimMayor = Nothing, -- this fails as this functor maps to Nothing and doesn't preserve the identity function
    interimMayor = f <$> (interimMayor oldDirectory),
    cabinet = f <$> cabinet oldDirectory,
    councilMembers = f <$> councilMembers oldDirectory
  }

-- implement an Arbitrary instance over the typeclass

instance Arbitrary a => Arbitrary (GovDirectory a) where
  arbitrary = do
    m <- arbitrary
    im <- arbitrary
    cab <- arbitrary
    cm <- arbitrary
    return $ GovDirectory
      { mayor = m
      , interimMayor = im
      , cabinet = cab
      , councilMembers = cm }

main :: IO ()
main = quickCheck govDirectoryFunctorCheck

govDirectoryFunctorCheck :: GovDirectory String -> Bool
govDirectoryFunctorCheck gd = fmap id gd == gd
