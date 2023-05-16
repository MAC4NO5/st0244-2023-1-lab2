module PRF where
import GHC.Natural (Natural)
data Nat = Zero | Succ Nat

-- Is a type of recursor used in all natural numbers
recNat :: a -> (Nat -> a -> a) -> Nat -> a
recNat a _ Zero = a
recNat a h (Succ n) = h n (recNat a h n)

-- Primitive recursive function to add two numbers
addR :: Nat -> Nat -> Nat
addR m n = recNat m (\_ y -> Succ y) n

-- Primitive recursive function to multiply two numbers
multiR :: Nat -> Nat -> Nat
multiR m n = recNat Zero (\_ y -> addR m y) n

--Primitive recursive function to get the identity of a number
idR :: Nat -> Nat
idR n = recNat n (\_ y -> y) Zero

-- Primitive recursive function that give the predeccesor of a number
predecessor :: Nat -> Nat
predecessor m = recNat Zero(\x _ -> x) m

-- Primitive recursive function that give the double Succ of a number
doubleSuccR :: Nat -> Nat
doubleSuccR m = recNat m(\_ y -> (Succ(Succ y)))(Succ Zero)

--Method to convert a Nat to Natural
natToNatural :: Nat -> Natural
natToNatural Zero = 0
natToNatural (Succ n) = 1 + natToNatural n

--Method to convert a Natural to Nat
naturalToNat :: Natural -> Nat
naturalToNat 0 = Zero
naturalToNat n = Succ (naturalToNat (n - 1))


