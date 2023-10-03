module Nat where
import Prelude hiding (sum, mult, exp, quot, min, gcd, lcm, div, max, pred, rem)


data Nat = O | S Nat
    deriving ( Eq , Show )


-- Soma
sum :: Nat -> Nat -> Nat
sum n O = n
sum n (S m) = S (sum n m)

-- Subtração
monus :: Nat -> Nat -> Nat
monus n O = n
monus O m = O
monus (S n) (S m) = monus n m

-- Multiplicação
mult :: Nat -> Nat -> Nat
mult n O = O
mult n (S m) = sum (mult m n) m

-- Exponenciação
exp :: Nat -> Nat -> Nat
exp n O = S O
exp n (S m) = mult n (exp n m)

-- Fatorial
fact :: Nat -> Nat
fact O = (S O)
fact (S n) = mult (S n) (fact n)

-- Fibonacci 
fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S (S n)) = sum (fib (S n)) (fib n)

-- Mínimo
min :: Nat -> Nat -> Nat
min n O = O
min (S n) (S m) = S(min n m)

-- Máximo
max :: Nat -> Nat -> Nat
max n O = n
max (S n) (S m) = S(max n m)

-- Dobro
double :: Nat -> Nat
double O = O
double (S n) = (S(S(double n)))

-- Quociente 
quot :: Nat -> Nat -> Nat
quot m n = quot' m n n 
  where
    quot' :: Nat -> Nat -> Nat -> Nat
    quot' O O k = S O
    quot' O m k = O
    quot' m O k = S (quot' m k k)
    quot' (S n) (S m) k = quot' n m k

-- Resto
rem :: Nat -> Nat -> Nat
rem O n = O
rem (S m) O = S(rem m O)
rem m n = rem' m(mult n (quot m n))
  where
    rem' :: Nat -> Nat -> Nat
    rem' (S m) (S n) = rem' m n 
    rem' m O = m

-- Divisão
div :: Nat -> Nat -> (Nat,Nat)
div n m = (quot n m, rem n m)

-- Máximo Divisor Comum
gcd :: Nat -> Nat -> Nat
gcd n O = n 
gcd n m = gcd m (rem n m)

-- Mínimo Multiplo Comum
lcm :: Nat -> Nat -> (Nat, Nat)
lcm n m = div (mult n m) (gcd n m)
