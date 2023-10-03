module ListNat where
import Nat
import Prelude hiding (elem, sumlist, product)

data ListNat = Empty | Cons Nat ListNat
    deriving ( Eq , Show )

--Tamanho da lista
lenght :: ListNat -> Nat
lenght Empty = O
lenght (Cons y ys) = S(lenght ys)

-- É elemento?
elem :: Nat -> ListNat -> Bool
elem _ Empty = False
elem x (Cons y ys) = x == y || elem x ys

-- Soma
--sumlist :: ListNat -> Nat
--sumlist Empty = O
--sumlist (Cons y ys) = sum y (sumlist ys)

-- Produto
product :: ListNat -> Nat
product Empty = (S O)
product (Cons y ys) = mult y (product ys)

