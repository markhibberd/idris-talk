module Data.TicTacToe

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

--%default total

%logging 1

data Player = PlayerX | PlayerO
data Cell = CellX | CellO | b

instance Show Cell where
  show CellX = "x"
  show CellO = "o"
  show b     = "_"

instance Show Player where
  show PlayerX = "x"
  show PlayerO = "o"

Pos : Type
Pos = Fin 9

Board : Type
Board = Vect 9 Cell

data PrettyBoard = PBoard Board

instance Show PrettyBoard where
  show (PBoard
    [nw, n,  ne,
     w,  c,  e,
     sw, s,  se]) =
     " " ++ show nw ++ " | " ++ show n  ++ " | " ++ show ne ++ "\n" ++
     "-----------\n" ++
     " " ++ show w  ++ " | " ++ show c  ++ " | " ++ show e  ++ "\n" ++
     "-----------\n" ++
     " " ++ show sw ++ " | " ++ show s  ++ " | " ++ show se ++ "\n"



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


nw : Pos
nw = 0

n : Pos
n = 1

ne : Pos
ne = 2

w : Pos
w = 3

c : Pos
c = 4

e : Pos
e = 5

sw : Pos
sw = 6

s : Pos
s = 7

se : Pos
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
occupied board = sum . map countOf $ board

next : Board -> Player
next board =
  if even (occupied board) then PlayerX else PlayerO

data ValidMove : Board -> Type where
  IsValidMove : Pos -> Player -> (b: Board) -> ValidMove b

validMove : Pos -> Player -> Board -> Bool
validMove position player board =
  (index position board) == b &&
    maybe True (\_ => False) (winner board) &&
      player == next board

validMoveX : Pos -> Player -> (b: Board) -> Maybe (ValidMove b)
validMoveX position player board =
  if validMove position player board then Just (IsValidMove position player board) else Nothing

doMove : ValidMove board -> Board
doMove (IsValidMove pos player board) =
  replaceAt pos (toCell player) board

data Game : Board -> Type where
  startGame : Game startBoard
  -- FIX consider adding "load game or similar" that would let me demo the search tactics etc...
  move : {board: Board} -> (validMove: ValidMove board) -> Game board -> Game (doMove validMove)


mv : {board: Board} -> (pos: Pos) -> (player: Player) -> (Game board) -> {default ItIsJust prf : (IsJust (validMoveX pos player board))} -> ValidMove board
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
state1 = move (mv ne PlayerX state0) state0
state1t = proof search

AMove : Type
AMove = (Pos, Player)

data TicTacToeRules : Effect where

  MoveIt : (pos : Pos) -> (player : Player) ->  { (Game board)  ==> {boardx} (Game boardx) } TicTacToeRules Board

  Get : { g ==> g } TicTacToeRules g

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


parse : String -> Maybe Pos
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


dostuff : String -> { [STDIO] ==> [STDIO] } Eff IO (Maybe Pos)
dostuff input =
  case parse input of
    Just xx => do putStrLn "Nice move!"
                  pure . Just $ xx
    Nothing => do putStrLn $ "Not a valid move " ++ trim input
                  pure Nothing


gamexx : Pos -> Player -> { [TICTACTOE (Game board)] ==> {outboard}
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
                do gamexx pp turn
--                                   ww <- isWeiner
--                                   pure (getBoard !Get)) pos

{--
         case updated == current of
           True => do putStrLn "Can't move there."
                      gamex
           False => case winner updated of
                      Nothing => gamex
                      Just winsy => do (putStrLn $ "Woot, we have a winner: " ++ winsy)

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
