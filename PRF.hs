data Nat = Zero | Succ Nat

instance Show Nat where
    show :: Nat -> String
    show Zero = "Zero"
    show (Succ n) = "Succ(" ++ show n ++ ")"

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
idP :: Nat -> Nat
idP n = recNat n (\_ y -> y) Zero

-- Primitive recursive function that give the predeccesor of a number
predecessor :: Nat -> Nat
predecessor Zero = Zero
predecessor (Succ m) = recNat m (\_ y -> y) Zero

--Primitive recursive function to multiply same number a X certain number times
pow :: Nat -> Nat -> Nat
pow m n= recNat (Succ Zero) (\_ y -> multiR m y) n
