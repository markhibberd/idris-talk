module TicTacType.Cli

import Effect.StdIO
import Effect.System

import TicTacType.Data
import TicTacType.Effect

printState : Board -> Player ->  { [STDIO] } Eff IO ()
printState board player = do
  putStrLn "Current game state ======"
  putStrLn ""
  putStrLn $ show board
  putStrLn ""
  putStrLn $ "It is " ++ show player ++ "'s turn."
  putStrLn ""

instance Default (TicTacToe InPlay) where
  default = T start

game : { [TICTACTOE InPlay, STDIO] ==> [TICTACTOE Done, STDIO] } Eff IO ()
game = do
  let current = !GetBoard
  let player = turn current
  let _ = !(printState current player)
  let input = !getStr
  case parse (trim input) of
    Nothing => do putStrLn $ "Sorry " ++ trim input ++ " is not a valid position"
                  pure !game
    Just position =>
               do InPlay <- Move position player | Done => putStrLn "Done"
                  game

go : { [TICTACTOE InPlay, STDIO] ==> [TICTACTOE Done, STDIO] } Eff IO ()
go = do
  game
  board <- GetBoard
  putStrLn $ show board
