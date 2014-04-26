module Data.TicTacToe

%default total

data Player = PlayerX | PlayerO
data Cell = CellX | CellO | b

toCell : Player -> Cell
toCell PlayerX = CellX
toCell PlayerO = CellO

instance Eq Player where
  PlayerX == PlayerX = True
  PlayerY == PlayerY = True
  PlayerX == PlayerY = False
  PlayerY == PlayerX = False

instance Eq Cell where
  CellX == CellX = True
  CellO == CellO = True
  b == b = True
  CellX == CellO = False
  CellX == b = False
  CellO == CellX = False
  CellO == b = False
  b == CellX = False
  b == CellO = False

Board : Type
Board = Vect 9 Cell

nw : Fin 9
nw = 0

n : Fin 9
n = 1

ne : Fin 9
ne = 2

w : Fin 9
w = 3

c : Fin 9
c = 4

e : Fin 9
e = 5

sw : Fin 9
sw = 6

s : Fin 9
s = 7

se : Fin 9
se = 8

startBoard : Board
startBoard =
  [b, b, b,
   b, b, b,
   b, b, b]

line : Cell -> Cell -> Cell -> Maybe Player
line CellX CellX CellX = Just PlayerX
line CellO CellO CellO = Just PlayerO
line _ _ _  = Nothing

winner : Board -> Maybe Player
winner
  [nw, n,  ne,
   w,  c,  e,
   sw, s,  se] =
  line ne n nw <|> line e c w <|> line se s sw <|> line ne e se <|>
  line n c s <|> line nw w sw <|> line ne c sw <|> line nw c se

countOf : Cell -> Nat
countOf b = 0
countOf _ = 1

even :  Nat -> Bool
even Z = True
even (S Z) = False
even (S n) = not (even n)

occupied : Board -> Nat
occupied = sum . map countOf

next : Board -> Player
next board =
  if even (occupied board) then PlayerX else PlayerO

validMove : Fin 9 -> Player -> Board -> Bool
validMove position player board =
  (index position board) == b &&
    maybe True (\_ => False) (winner board) &&
      player == next board

data Game : Board -> Type where
  startGame : Game startBoard
  -- FIX consider adding "load game or similar" that would let me demo the search tactics etc...
  move : (pos : Fin 9) -> (val : Player) -> {board: Board} -> (game: Game board) -> {default oh ok : so (validMove pos val board)} ->
         Game (replaceAt pos (toCell val) board)

started : Board -> Bool
started board =
  (sum . map countOf $ board) > 0

prevBoard : {board : Board} -> (g : Game board) -> {default oh ok : so (started board)} -> Board
prevBoard startGame = startBoard
prevBoard (move pos val {board} game) = board

takeBack : {board : Board} -> (g : Game board) -> {default oh ok : so (started board)} -> Game (prevBoard g {ok})
takeBack startGame = startGame
takeBack (move pos val {board} game) =  game

getBoard : {board : Board} -> (g : Game board) -> Board
getBoard {board} _ = board

state0 : ?state0t
state0 = startGame
state0t = proof search

--state0x : ?state0xt
--state0x = takeBack state0
--state0xt = proof search

state1 : ?state1t
state1 = move ne PlayerX state0
state1t = proof search
