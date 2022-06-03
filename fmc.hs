module Main where

import Prelude hiding ((+), (-), (*), (^))

data Nat = Zero | S Nat
    deriving (Show)

(+) :: Nat -> Nat -> Nat
n + Zero = n
n + (S m) = S(n + m)

pd :: Nat -> Nat
pd Zero = Zero
pd (S n) = n

(-) :: Nat -> Nat -> Nat
n - Zero = n
n - (S m) = pd (n - m)

(*) :: Nat -> Nat -> Nat
n * Zero = Zero
n * (S m) = n * m + n

(^) :: Nat -> Nat -> Nat
n^Zero = S Zero
n^(S m) = (n ^ m) * n

double :: Nat -> Nat
double Zero = Zero
double (S n) = S(S (double n))

fact :: Nat -> Nat
fact Zero = S Zero
fact (S n) = fact n * S n

fib :: Nat -> Nat
fib Zero = Zero
fib (S Zero) = S Zero
fib (S (S n)) = fib n + fib (S n)

main :: IO ()
main = do
    
    