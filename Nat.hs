module Nat where
import Prelude hiding (sum, mult, exp, quot, min, gcd, lcm, div, max, pred, rem)


data Nat = O | S Nat
    deriving ( Eq , Show )

-- Soma
sum :: Nat -> Nat -> Nat
sum n O = n
sum n (S m) = S (sum n m)

-- Multiplicação
mul :: Nat -> Nat -> Nat
mul n O = O
mul n (S m) = sum (mul m n) m

-- Exponenciação
exp :: Nat -> Nat -> Nat
exp n O = S O
exp n (S m) = mul n (exp n m)

-- Fatorial
fact :: Nat -> Nat
fact O = (S O)
fact (S n) = mul (S n) (fact n)

-- Fibonacci 
fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S (S n)) = sum (fib (S n)) (fib n)

