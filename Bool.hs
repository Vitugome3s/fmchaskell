module Bool where
import Nat
import Prelude hiding (if_then_else, leq, (==), False, True, Bool, ev, od, isMul3, rem, eq)

data Bool = False | True
    deriving ( Eq , Show )

if_then_else_ :: Bool -> Nat -> Nat -> Nat
if_then_else_ False n m = n
if_then_else_ True n m = m 

-- Menor ou igual que
leq :: Nat -> Nat -> Bool
leq O m = True
leq n O = False
leq (S n) (S m) = leq n m

-- Igualdade
eq :: Nat -> Nat -> Bool   
eq O O = True
eq (S n) (S m) = eq n m
eq _ _ = False

-- Par
ev :: Nat -> Bool
ev O = True
ev (S O) = False
ev (S(S n)) = ev n

-- Ímpar 
od :: Nat -> Bool
od O = False
od (S O) = True
od (S n) = od n

-- Divide
divides :: Nat -> Nat -> Bool
divides O O = True
divides m O = False
divides m n = divides O (rem m n)

-- É multiplo de 3
isMul3 :: Nat -> Bool
isMul3 O = True
isMul3 (S(S(S(n)))) = isMul3 n
isMul3 _ = False

-- É zero
isZero :: Nat -> Bool
isZero O = True
isZero _ = False

-- If then else bool

if_then_else_2 :: Bool -> Bool -> Bool -> Bool
if_then_else_2 True n _ = n
if_then_else_2 False _ m = m