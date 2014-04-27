module Data.Support

%default total

%logging 0

even :  Nat -> Bool
even Z = True
even (S Z) = False
even (S n) = not (even n)
