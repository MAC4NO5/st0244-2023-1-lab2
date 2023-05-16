import PRF
import Test.QuickCheck
import Test.QuickCheck.Instances.Natural ()
import GHC.Natural (Natural)

--Properties of add function, return a number added with other
prop_addR :: Natural -> Natural -> Bool
prop_addR m n = natToNatural(addR m' n') == m + n
    where
    m', n':: Nat
    m' = naturalToNat m
    n' = naturalToNat n

--Properties of multiply number, return a number multiply with other
prop_multiR :: Natural -> Natural -> Bool
prop_multiR m n = natToNatural(multiR m' n') == m * n
    where
    m', n':: Nat
    m' = naturalToNat m
    n' = naturalToNat n

--Properties of identity number, return the same number
prop_idR :: Natural -> Bool
prop_idR n = natToNatural(idR n') == n
    where
    n' :: Nat
    n' = naturalToNat n

--Properties of predeccessor of a number, return a previous number
prop_predeccesor :: Natural -> Bool
prop_predeccesor n
    | n == 0 = True
    | otherwise = natToNatural(predecessor n') == n - 1
    where
    n' :: Nat
    n' = naturalToNat n

--Properties of doubleSucc of a number, return a number with double succ
prop_doubleSuccR :: Natural -> Bool
prop_doubleSuccR n = natToNatural(doubleSuccR n') == n + 2
    where
    n' :: Nat
    n' = naturalToNat n

--Here our tests run
main :: IO ()
main = do
    quickCheck prop_addR
    quickCheck prop_multiR
    quickCheck prop_idR
    quickCheck prop_predeccesor
    quickCheck prop_doubleSuccR