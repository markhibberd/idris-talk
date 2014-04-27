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

--game : { [TICTACTOE inb, STDIO] ==> {outb} [TICTACTOE outb, STDIO] } Eff IO Board
game : { [TICTACTOE inb, STDIO] ==> {outb} [TICTACTOE outb, STDIO] } Eff IO Board
game = do
  let current = !GetBoard
  let player = turn current
  let _ = !(printState current player)
  let input = !getStr
  case parse (trim input) of
    Nothing => do putStrLn $ "Sorry " ++ trim input ++ " is not a valid position"
                  pure !game
    Just position =>
               do putStrLn $ "Trying to play move " ++ trim input
                  GetBoard

--                  pure !(Move position player)
