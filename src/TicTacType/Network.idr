module TicTacType.Network

import Effect.StdIO
import Effect.System
import Effect.State

import IdrisNet.Socket
import IdrisNet.TCP.TCPServer

import TicTacType.Data
import TicTacType.Effect

%logging 0


data CurrentGame : Type where
  Current : (TicTacToe state) -> CurrentGame

instance Default CurrentGame where
  default = Current . T $ start

instance Show CurrentGame where
  show (Current (T g)) = show . toBoard $ g

data Command =
    ShowGame
  | HelpGame
  | MoveGame TicTacType.Data.Position

total
parseCommand : String -> Maybe Command
parseCommand s = case unpack s of
  's' :: 'h' :: 'o' :: 'w' :: Nil => Just ShowGame
  'h' :: 'e' :: 'l' :: 'p' :: Nil => Just HelpGame
  'm' :: 'o' :: 'v' :: 'e' :: ' ' :: position => map MoveGame (parse $ pack position)
  _ => Nothing

handleCommand : CurrentGame -> String -> String
handleCommand current s = case parseCommand s of
  Nothing => "Not a valid command: " ++ s
  Just ShowGame => show current
  Just HelpGame => "something helpful"
  Just (MoveGame p) => "some move: "

receive' : { [STDIO, STATE CurrentGame, TCPSERVERCLIENT ClientConnected] ==> {n}
             [STDIO, STATE CurrentGame, TCPSERVERCLIENT ()] } Eff IO ()
receive' = do
  -- Receive
  putStrLn "Waiting...."
  current <- get
  OperationSuccess (str, len) <- tcpRecv 1024
    | RecoverableError _ => receive'
    | FatalError err => do putStr ("Error receiving: " ++ (show err))
                           tcpFinalise
    | ConnectionClosed => return ()
  OperationSuccess _ <- tcpSend (handleCommand current (trim str))
    | RecoverableError err => do putStr ("Error sending: " ++ (show err))
                                 tcpClose
    | FatalError err => do putStr ("Error sending: " ++ (show err))
                           tcpFinalise
    | ConnectionClosed => return ()
  put current
  let c = !get
  putStrLn $ "You have received " ++ show c ++ " messages"
  receive'

receive : ClientProgram ()
receive = new . new $ receive'

forkServerLoop : { [TCPSERVER (ServerListening), STDIO] ==>
               [TCPSERVER (), STDIO] } Eff IO ()
forkServerLoop = do
  -- Accept, and perform the "receive" program with the new socket.
  OperationSuccess _ <- forkAccept receive
       | RecoverableError _ => forkServerLoop
       | FatalError err => do putStr ("Error accepting: " ++ (show err))
                              finaliseServer
       | ConnectionClosed => return ()
  forkServerLoop

serverLoop : { [TCPSERVER (ServerListening), STDIO] ==>
               [TCPSERVER (), STDIO] } Eff IO ()
serverLoop = do
  -- Accept, and perform the "receive" program with the new socket.
  OperationSuccess _ <- accept receive
    | RecoverableError _ => serverLoop
    | FatalError err => do putStr ("Error accepting: " ++ (show err))
                           finaliseServer
    | ConnectionClosed => return ()
  serverLoop

setupServer : Port -> Bool ->
              { [TCPSERVER (), STDIO] } Eff IO ()
setupServer port do_fork = do
  putStr "Binding\n"
  OperationSuccess _ <- bind Nothing port
    | RecoverableError _ => return ()
    | FatalError err => do putStr ("Error binding: " ++ (show err) ++ "\n")
                           return ()
    | ConnectionClosed => return ()
  putStr "Bound\n"
  OperationSuccess _ <- listen
    | RecoverableError err => do putStr ("Recoverable error: " ++ (show err))
                                 closeBound
    | FatalError err => do putStr ("Error binding: " ++ show err)
                           finaliseServer
    | ConnectionClosed => return ()
  putStr "Listening\n"
  if do_fork then forkServerLoop else serverLoop


partial
go : IO ()
go = run (setupServer 1236 True)
