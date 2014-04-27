module TicTacType.Support

%default total

%logging 0

-- TODO get rid of this helper

even :  Nat -> Bool
even Z = True
even (S Z) = False
even (S n) = not (even n)
