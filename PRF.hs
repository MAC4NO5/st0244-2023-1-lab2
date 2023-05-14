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

predecesor :: Nat -> Nat
predecesor m = recNat Zero (\_ y -> m) m