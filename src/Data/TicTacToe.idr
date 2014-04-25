module Data.TicTacToe

--import Effect.System

data Player =
 X | O

other : Player -> Player
other X = O
other O = X

data Cell =
 F Player | E

x : Cell
x = F X

o : Cell
o = F O

e : Cell
e = E

countOf : Cell -> Nat
countOf (F _) = 1
countOf E = 0

data State =
  Playing Player | Finished | NotStarted

data Board : Nat -> Type where
  B : (repr: Vect 9 Cell) -> Board (sum . map countOf $ repr)

line : Cell -> Cell -> Cell -> Maybe Player
line (F X) (F X) (F X) = Just X
line (F O) (F O) (F O) = Just O
line _ _ _ = Nothing

winner : Board n -> Maybe Player
winner (B (ne :: n  :: nw ::
           e  :: c  :: w  ::
           se :: s  :: sw :: Nil)) =
   line ne n nw <|> line e c w <|> line se s sw <|>
   line ne e se <|> line n c s <|> line nw w sw <|>
   line ne c sw <|> line nw c se

total
occupied : {n : Nat} -> Board n -> Nat
occupied {n} _ = n

total
full : Board n -> Bool
full b = occupied b == 9

total
even :  Nat -> Bool
even Z = True
even (S Z) = False
even (S n) = not (even n)

total
nToPlayer :  Nat -> Player
nToPlayer n = (if (even n) then X else O)

example : Board ?wlen
example =
  B (x :: o :: x ::
     e :: e :: e ::
     e :: e :: e :: Nil)

wlen = proof search

{-
data Board : State -> Type where
  Init : Board NotStarted
  Won : Player -> Board Finished
  Draw : Board Finished
  InProgress :
    (p: Player) ->
    (ne : Cell) -> (n  : Cell) -> (nw : Cell) ->
    (e  : Cell) -> (c  : Cell) -> (w  : Cell) ->
    (se : Cell) -> (s  : Cell) -> (sw : Cell) ->
    Board (Playing (other p))
-}
{-
data Game : Effect where

  MoveX : (Board ) ->
-}
