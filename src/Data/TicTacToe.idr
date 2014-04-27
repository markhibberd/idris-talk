module Data.TicTacToe

import Data.Support

import Effect.StdIO
import Effect.System
import Effect.Random

{-

TODO make Board a newtype with a decent show
TODO make a "ValidBoard" proof that can be used to load boards
TODO network??
TODO ui??
TODO CellX/CellY -> Occupied
TODO b -> Empty
TODO make board effects return "Maybe Board"
-}

%default total

%logging 0

-----------------------------------------------------------------------
-- TIC TAC TYPE
-----------------------------------------------------------------------

{- This program represents a game of Tic Tac Type in Idris, really
   it should be called Naughts and Crosses, but we shall let this
   language attrocity slide in the name of a good pun. -}

-----------------------------------------------------------------------
-- PLAYERS
-----------------------------------------------------------------------

{- There are two players, X and O, who each play in turn. -}

data Player = X | O

instance Show Player where
  show X = "x"
  show O = "o"

instance Eq Player where
  X == X = True
  O == O = True
  X == O = False
  O == X = False


-----------------------------------------------------------------------
-- CELLS
-----------------------------------------------------------------------

{- A cell is a position in the game. A cell may be either occupied by a
   player or unoccupied -}


data Cell = Occupied Player | Unoccupied

instance Show Cell where
  show (Occupied p) = show p
  show Unoccupied     = "_"

instance Eq Cell where
  (Occupied a) == (Occupied b) = a == b
  (Occupied _) == Unoccupied = False
  Unoccupied == (Occupied _) = False
  Unoccupied == Unoccupied = True


-----------------------------------------------------------------------
-- POSITIONS
-----------------------------------------------------------------------

{- In our default 3x3 game, positions are the finite set of naturals less
   than 9, which index onto the board from left to right, top to bottom.
   i.e.

     0 | 1 | 2
    -----------
     3 | 4 | 5
    -----------
     6 | 7 | 8

 -}


Position : Type
Position = Fin 9

{- But given that this is kind of confusing way to deal with identifying
   a position, we also have these convenience definitions that let us
   treat a co-ordinate as a literal position.
   i.e.

     nw | n | ne
    -------------
     w  | c | e
    -------------
     sw | s | se

 -}


{- North-West, or the Top-Left corner position. -}
nw : Position
nw = 0

{- North, or the Top-Center position. -}
n : Position
n = 1

{- North-East, or the Top-Right corner position. -}
ne : Position
ne = 2

{- West, or the Left-Center position. -}
w : Position
w = 3

{- Center position. -}
c : Position
c = 4

{- East, or the Right-Center position. -}
e : Position
e = 5

{- South-West, or the Bottom-Left corner position. -}
sw : Position
sw = 6

{- South, or the Bottom-Center position. -}
s : Position
s = 7

{- South-East, or the Bottom-Right corner position. -}
se : Position
se = 8


-----------------------------------------------------------------------
-- THE BOARD
-----------------------------------------------------------------------

{- The board is a 3x3 grid represented as a Vector of length 9. This
   limits us to the default size, however it could easily be extended
   with limited effect on the rest of the program (excepting the algorithm
   that determines a win). -}

abstract
data Board = B (Vect 9 Cell)

instance Eq Board where
  (B a) == (B b) = a == b

instance Show Board where
  show (B [nw, n,  ne,
           w,  c,  e,
           sw, s,  se]) =
     " " ++ show nw ++ " | " ++ show n  ++ " | " ++ show ne ++ "\n" ++
     "-----------\n" ++
     " " ++ show w  ++ " | " ++ show c  ++ " | " ++ show e  ++ "\n" ++
     "-----------\n" ++
     " " ++ show sw ++ " | " ++ show s  ++ " | " ++ show se ++ "\n"

{- Next, we will define a bunch of useful combinators for working with
   boards. Conveniently we get to define these at the value level, even
   though we will end up using them on the type level most of the time.  -}

-- Convert a board back to it's vectorized form
toVect : Board -> Vect 9 Cell
toVect (B v) = v

-- An intial empty board
empty : Board
empty =
  B [Unoccupied, Unoccupied, Unoccupied,
     Unoccupied, Unoccupied, Unoccupied,
     Unoccupied, Unoccupied, Unoccupied]

-- Do we have 3 in a row?
match : Cell -> Cell -> Cell -> Maybe Player
match (Occupied a) (Occupied b) (Occupied c) = toMaybe (a == b && b == c) a
match _ _ _  = Nothing

-- Is there a winner on the board?
winner : Board -> Maybe Player
winner (B [nw, n,  ne,
           w,  c,  e,
           sw, s,  se]) =
  match ne n nw <|> match e  c w  <|> match se s sw <|> match ne e se <|>
  match n  c s  <|> match nw w sw <|> match ne c sw <|> match nw c se

-- Is this a valid board?
isValidBoard : Board -> Bool
isValidBoard board =
  let xs = sum . map xToInt . toVect $ board in
  let ys = sum . map oToInt . toVect $ board in
  xs == ys || xs == (S ys) where

  xToInt Unoccupied = 0
  xToInt (Occupied X) = 1
  xToInt (Occupied O) = 0

  oToInt Unoccupied = 0
  oToInt (Occupied X) = 0
  oToInt (Occupied O) = 1

-- How many positions are occupied ?
occupied : Board -> Nat
occupied board =
  sum . map toInt . toVect $ board where

  toInt Unoccupied = 0
  toInt (Occupied _) = 1

-- Who's turn is it?
turn : Board -> Player
turn board =
  if even (occupied board) then X else O

-- Is this position free?
free : Position -> Board -> Bool
free position (B board) =
  index position board == Unoccupied

-- Is this a valid move for the current board?
isValidMove : Position -> Player -> Board -> Bool
isValidMove position player board =
  free position board && isJust (winner board) && turn board == player

{- We want a data structure to carry the proof that a move is valid for a given
   board, so we can use it later on. This is more useful than the Bool above. -}

data ValidMove : Board -> Type where
  IsValidMove : Position -> Player -> (board: Board) -> ValidMove board

{- To construct these, we first build a function that "might" produce a ValidMove
   depending on the constraints of `isValidMove`. -}

tryValidMove : Position -> Player -> (board: Board) -> Maybe (ValidMove board)
tryValidMove position player board =
  toMaybe (isValidMove position player board) (IsValidMove position player board)

{- But the great thing about all these types, is that we can directly construct a
   valid move if we can prove it. -}

validMove : (position: Position) -> (player: Player) -> (board: Board) ->
            {default ItIsJust prf : (IsJust (tryValidMove position player board))} -> ValidMove board
validMove position player board {prf} with (tryValidMove position player board)
  validMove position player board {prf = ItIsJust} | Just y = y

-- Lets run a validate move and produce a new board.

runMove : ValidMove board -> Board
runMove (IsValidMove position player (B board)) =
  B $ replaceAt position (Occupied player) board


{- Now a similar treatment to create a safe board constructor. -}

tryBoard : Vect 9 Cell -> Maybe Board
tryBoard vect = let b = B vect in toMaybe (isValidBoard b) b

board : (vect: Vect 9 Cell) -> {default ItIsJust prf : (IsJust (tryBoard vect))} -> Board
board vect {prf} with (tryBoard vect)
  board vect {prf = ItIsJust} | Just y = y


-----------------------------------------------------------------------
-- THE GAME
-----------------------------------------------------------------------

{- Now we want to build a Game data type that will hold our game state.
   Importantly, we are now lifting the "Board" to the type level, which
   means that the type of a game carries around the entire board state. -}

data Game : Board -> Type where

  -- start a new game with an empty board
  start : Game empty

  -- start from some arbitrary starting point
  load : (b: Board) -> Game b

  -- make a (guaranteed to be valid) move on the current game
  move : {b: Board} -> (m: ValidMove b) -> Game b -> Game (runMove m)

{--

data Game : Board -> Type where
  startGame : Game startBoard
  -- FIX consider adding "load game or similar" that would let me demo the search tactics etc...
  move : {board: Board} -> (validMove: ValidMove board) -> Game board -> Game (doMove validMove)


mv : {board: Board} -> (pos: Position) -> (player: Player) -> (Game board) -> {default ItIsJust prf : (IsJust (validMoveX pos player board))} -> ValidMove board
mv {board} pos player game {prf} with (validMoveX pos player board)
  mv {board} pos player game {prf = ItIsJust} | Just y = y

started : Board -> Bool
started board =
  (sum . map countOf $ board) > 0

{--
prevBoard : {board : Board} -> (g : Game board) -> {default oh ok : so (started board)} -> Board
prevBoard startGame = startBoard
prevBoard (move pos val {board} game) = board

takeBack : {board : Board} -> (g : Game board) -> {default oh ok : so (started board)} -> Game (prevBoard g {ok})
takeBack startGame = startGame
takeBack (move pos val {board} game) =  game
--}

getBoard : {board : Board} -> (g : Game board) -> Board
getBoard {board} _ = board

state0 : ?state0t
state0 = startGame
state0t = proof search

--state0x : ?state0xt
--state0x = takeBack state0
--state0xt = proof search

state1 : ?state1t
state1 = move (mv ne X state0) state0
state1t = proof search

AMove : Type
AMove = (Position, Player)

data TicTacToeRules : Effect where

  MoveIt : (pos : Position) -> (player : Player) ->  { (Game board)  ==> {boardx} (Game boardx) } TicTacToeRules Board

  Get : { g ==> g } TicTacToeRules g

{--

TICTACTOE : Type -> EFFECT
TICTACTOE g = MkEff g TicTacToeRules

using (m : Type -> Type)
  instance Handler TicTacToeRules m where
    handle game (MoveIt pos player) k =
        case validMoveX pos player (getBoard game) of
          Just myMove => k (doMove myMove) (move myMove game)
          Nothing => k (getBoard game) game

    handle st Get k = k st st

{--

game : { [TICTACTOE b1, SYSTEM, STDIO] ==>
         [TICTACTOE b2, SYSTEM, STDIO] } Eff IO ()
game = do putStrLn (show (PrettyBoard (getBoard !Get)))
          pure ()

--}

instance Default (Game startBoard) where
  default = startGame


game : { [TICTACTOE (Game z)] ==>
         [TICTACTOE (Game z)] } Eff IO Board
game =  do gg <- Get
           pure (getBoard gg)

{--
gamex : { [TICTACTOE (Game startBoard), STDIO] ==>
          [TICTACTOE (Game startBoard), STDIO] } Eff IO Board
gamex =  do gg <- Get
            let bb = getBoard gg
            let ss = show (PBoard bb)
            let x = !(putStrLn ss)
            pure bb
--}


parse : String -> Maybe Position
parse "nw" = Just nw
parse "n"  = Just n
parse "ne" = Just ne
parse "e"  = Just e
parse "c"  = Just c
parse "w"  = Just w
parse "sw" = Just sw
parse "s"  = Just s
parse "se" = Just se
parse _    = Nothing

{--
dostuff : String -> Player -> (b: Board) -> { [STDIO] ==> [STDIO] } Eff IO (Maybe (ValidMove b))
dostuff input player board =
  case parse input of
    Just xx => do putStrLn "Nice move!"
                  pure $ validMoveX xx player board
    Nothing => do putStrLn $ "Not a valid move " ++ trim input
                  pure Nothing
-}


dostuff : String -> { [STDIO] ==> [STDIO] } Eff IO (Maybe Position)
dostuff input =
  case parse input of
    Just xx => do putStrLn "Nice move!"
                  pure . Just $ xx
    Nothing => do putStrLn $ "Not a valid move " ++ trim input
                  pure Nothing


gamexx : Position -> Player -> { [TICTACTOE (Game board)] ==> {outboard}
          [TICTACTOE (Game outboard)] } Eff IO Board
gamexx pos player = MoveIt pos player

isWeiner : { [TICTACTOE (Game board)] } Eff IO Bool
isWeiner  = pure (isJust (winner (getBoard  !Get)))


--gamex : { [TICTACTOE (Game startBoard), STDIO] ==> {outboard}
--          [TICTACTOE (Game outboard), STDIO] } Eff IO Board
gamex : { [TICTACTOE (Game someboard), STDIO] ==> {outboard}
          [TICTACTOE (Game outboard), STDIO] } Eff IO Board
gamex =  do putStrLn "Current game state ======"
            let current = getBoard !Get
            let pretty = show (PBoard current)
            let turn = next current
            putStrLn pretty
            putStrLn $ "\n Make a move " ++ show turn

            input <- getStr
            pos <- dostuff (trim input)
            case pos of
              Nothing => gamex
              Just pp =>
                gamexx pp turn
{--
  where
    handlage : (board: Board) ->
               { [TICTACTOE (Game board), STDIO] ==> {outboard}
                 [TICTACTOE (Game outboard), STDIO] } Eff IO Board
    handlage current =
      do let updated = getBoard !Get
         case updated == current of
           True => do putStrLn "Can't move there."
                      gamex
           False => pure (getBoard !Get) --case winner updated of
--                      Nothing => gamex
--                      Just winsy => gamex -- pure updated

--}

--gamex --do --putStrLn ("Woot, we have a winner: " ++ winsy)
--                                       pure (getBoard !Get)



--                                   ww <- isWeiner
--                                   pure (getBoard !Get)) pos

{--

--}
--





{--         dostuff
            case parse (trim input) of
              Just pos => do putStrLn "Nice move!"
                             --let nu = !(MoveIt pos turn) in
                             pure ()
              Nothing => do putStrLn $ "Not a valid move: " ++ trim input
--                            gamex
                            pure ()
--}
--            pure ()

--g0;amex : { [TICTACTOE b1, STDIO] ==>
--         [TICTACTOE b1, STDIO] } Eff IO ()
--gamex =
--  do putStrLn (show (PrettyBoard !Get))
--     Get




go : IO ()
go = putStrLn "xoxoxox"



{--
data Expr = Val Integer
  | Var String
  | Add Expr Expr
  | Random Integer


eval : Expr -> { [EXCEPTION String, RND, STATE Env] } Eff m Integer
eval (Random upper) = do val <- rndInt 0 upper
                         putStrLn (show val)
                         return val
--}

--}

--}
