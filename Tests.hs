import PRF
import Test.QuickCheck
import Test.QuickCheck.Instances.Natural ()
import GHC.Natural (Natural)

--Properties of add function to use tests of QuickCheck
prop_addR :: Natural -> Natural -> Bool
prop_addR m n = natToNatural(addR m' n') == m + n
    where
    m', n':: Nat
    m' = naturalToNat m
    n' = naturalToNat n

--Properties of add function to use tests of QuickCheck
prop_multiR :: Natural -> Natural -> Bool
prop_multiR m n = natToNatural(multiR m' n') == m * n
    where
    m', n':: Nat
    m' = naturalToNat m
    n' = naturalToNat n

--Properties of add function to use tests of QuickCheck
prop_idR :: Natural -> Bool
prop_idR n = natToNatural(idR n') == n
    where
    n' :: Nat
    n' = naturalToNat n

prop_predeccesor :: Natural -> Bool
prop_predeccesor n
    | n == 0 = True
    | otherwise = natToNatural(predecessor n') == n - 1
    where
    n' :: Nat
    n' = naturalToNat n

prop_doubleSuccR :: Natural -> Bool
prop_doubleSuccR n = natToNatural(doubleSuccR n') == n + 2
    where
    n' :: Nat
    n' = naturalToNat n
    --ghc -o lab2 -package quickcheck-instances Tests.hs
main :: IO ()
main = do
    quickCheck prop_addR
    quickCheck prop_multiR
    quickCheck prop_idR
    quickCheck prop_predeccesor
    quickCheck prop_doubleSuccR