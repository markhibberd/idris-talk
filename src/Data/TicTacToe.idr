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
