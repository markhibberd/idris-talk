module TicTacType.Effect

import Effect.System
import TicTacType.Data

%default total

%logging 0

data TicTacToeState =
  InPlay | Draw | VictoryTo Player

toState : Board -> TicTacToeState
toState b =
   case (winner b, complete b) of
     (Just p, _)  => VictoryTo p
     (Nothing, True)  => Draw
     (Nothing, False) => InPlay

data TicTacToe : TicTacToeState -> Type where
  T : (Game b) -> TicTacToe (toState b)

data TicTacToeRules : Effect where
  Move : (position : Position) -> (player : Player) -> { TicTacToe st  ==> {st'} (TicTacToe st')  } TicTacToeRules TicTacToeState

  Get : { g } TicTacToeRules g

TICTACTOE : TicTacToeState -> EFFECT
TICTACTOE s = MkEff (TicTacToe s) TicTacToeRules

using (m : Type -> Type)
  instance Handler TicTacToeRules m where
    handle (T game) (Move position player) k =
      case tryValidMove position player (toBoard game) of
        Just m' => let updated = move' m' game in
                   k (toState . toBoard $ updated) (T updated)
        Nothing => k (toState . toBoard $ game) (T game)

    handle t Get k = k t t

{- An extra data type to represent the result of a move -}

{--
data MoveResult = MoveOk | MoveError | Winner Player | Draw

data TicTacToeRules : Effect where
  Move : (position : Position) -> (player : Player) -> { (Game old)  ==> {board} (Game board)  } TicTacToeRules Board
  Get : { g } TicTacToeRules g

TICTACTOE : Board -> EFFECT
TICTACTOE b = MkEff (Game b) TicTacToeRules

using (m : Type -> Type)
  instance Handler TicTacToeRules m where
    handle game (Move position player) k =
      case tryValidMove position player (toBoard game) of
        Just m' => let updated = move' m' game in
                   k (toBoard updated) updated
        Nothing => k (toBoard game) game

    handle game Get k = k game game

  GetBoard : { [TICTACTOE b] } Eff m Board
  GetBoard =  pure $ toBoard !Get

  -- This is dodge, but there is something preventing a more obvious
  -- solution (probably me missing something). The solution probably
  -- lines in being able to get this to work: https://gist.github.com/markhibberd/11345304

  CheckMove : Board -> { [TICTACTOE b] } Eff m MoveResult
  CheckMove old =
   case (!GetBoard == old, winner !GetBoard, complete !GetBoard) of
     (True, _, _)     => pure MoveError
     (_, Just p, _)  => pure . Winner $ p
     (_, Nothing, True)  => pure Draw
     (_, Nothing, False) => pure MoveOk
--}
