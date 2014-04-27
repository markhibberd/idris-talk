module TicTacType.Effect

import Effect.System
import Effect.StdIO

{--
data Capture : Effect where
  Try : { old ==> {the (Maybe Nat) ok} (case ok of
                                        Just n => Vect n String
                                        Nothing => old) } Capture (Maybe Nat)
--}

{--
data Capture : Effect where
  Try : { old ==> {updated} (Vect updated String) } Capture Nat

CAPTURE : Nat -> EFFECT
CAPTURE n = MkEff (Vect n String) Capture

using (m : Type -> Type)
  instance Handler TicTacToeRules m where
    handle game (Move position player) k =
      case tryValidMove position player (toBoard game) of
        Just m' => let updated = move' m' game in
                   k (toBoard updated) updated
        Nothing => k (toBoard game) game

run : { [CAPTURE n, STDIO] => {m} [CAPTURE m, STDIO] } Eff IO Nat
run = do putStrLn "enter a string"
         str <- getStr
         if str == "magic =
--}
