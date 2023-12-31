module ListNat where
import Nat
import Bool
import Prelude hiding (elem, sumlist, product, (++), append, product, rem, True, Bool, False, reverse, sum, mult,exp)

data ListNat = Empty | Cons Nat ListNat
    deriving ( Eq , Show )

--Tamanho da lista
lenght :: ListNat -> Nat
lenght Empty = O
lenght (Cons y ys) = S(lenght ys)

-- É elemento?
elem :: Nat -> ListNat -> Bool
elem _ Empty = False
elem x (Cons y ys) = if (x == y) then True else (elem x ys)

-- Soma
sumlist :: ListNat -> Nat
sumlist Empty = O
sumlist (Cons y ys) = sum y (sumlist ys)

-- Produto
product :: ListNat -> Nat
product Empty = (S O)
product (Cons y ys) = mult y (product ys)

-- Concatenação
(++) :: ListNat -> ListNat -> ListNat
Empty ++ ys = ys
(Cons x xs) ++ ys = (Cons x (xs ++ ys))

-- Reverso
reverse :: ListNat -> ListNat
reverse Empty = Empty
reverse (Cons y ys) = (reverse ys) ++ (Cons y Empty)

-- Todos são pares?
allEven :: ListNat -> Bool
allEven Empty = True  
allEven (Cons y ys) = if_then_else_2 (ev y) (allEven ys) False

-- Algum par?
anyEven :: ListNat -> Bool
anyEven Empty = False  
anyEven (Cons y ys) = if_then_else_2 (ev y) True (anyEven ys)

-- Todos são impares?
allOd :: ListNat -> Bool
allOd Empty = True  
allOd (Cons y ys) = if_then_else_2 (od y) (allOd ys) False

-- Algum impar?
anyOd :: ListNat -> Bool
anyOd Empty = False  
anyOd (Cons y ys) = if_then_else_2 (od y) True (anyOd ys)

--- Todos Zero?
allZero :: ListNat -> Bool
allZero Empty = True
allZero (Cons y ys) = if_then_else_2 (isZero y) (allZero ys) False

-- Algum Zero?
anyZero :: ListNat -> Bool
anyZero Empty = False  
anyZero (Cons y ys) = if_then_else_2 (isZero y) True (allZero ys)

-- Adicionar nat
addNat :: Nat -> ListNat -> ListNat
addNat y Empty = Empty
addNat y (Cons x xs) = Cons (sum y x) (addNat y xs)

-- Multiplicar nat
multNat :: Nat -> ListNat -> ListNat
multNat y Empty = Empty
multNat y (Cons x xs) = Cons (mult y x) (multNat y xs)

-- exponencial nat
expNat :: Nat -> ListNat -> ListNat
expNat y Empty = Empty
expNat y (Cons x xs) = Cons (exp y x) (expNat y xs)







