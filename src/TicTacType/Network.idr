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

game : CurrentGame -> (x ** Game x)
game (Current (T z)) =
  (toBoard z ** z)

--toBoard' : CurrentGame -> (x ** Board)
--toBoard' c =
--  case game c of
--    (x ** g) => (x ** toBoard g)
--  case x of
--    T game => toBoard game

instance Default CurrentGame where
  default = Current . T $ start

instance Show CurrentGame where
  show (Current (T g)) = show . toBoard $ g

data Command =
    ShowGame
  | HelpGame
  | MoveGame Player TicTacType.Data.Position

total
parseCommand : String -> Maybe Command
parseCommand s = case unpack s of
  's' :: 'h' :: 'o' :: 'w' :: Nil => Just ShowGame
  'h' :: 'e' :: 'l' :: 'p' :: Nil => Just HelpGame
  'm' :: 'o' :: 'v' :: 'e' :: ' ' :: 'x' :: ' ' :: position =>
    map (MoveGame X) (parse $ pack position)
  'm' :: 'o' :: 'v' :: 'e' :: ' ' :: 'o' :: ' ' :: position =>
    map (MoveGame O) (parse $ pack position)
  _ => Nothing


handleCommand : String -> { [STATE CurrentGame] } Eff IO String
handleCommand s = --do
--  let current = !get
  case parseCommand s of
    Nothing =>
      pure $ "Not a valid command: " ++ s ++  "\n"
    Just ShowGame =>
      pure $ show !get
    Just HelpGame =>
      pure "Commands: show | help | move {player} {position}\n"
    Just (MoveGame player pos) => do
      let current = !get
      let (b ** g) = game current
      case tryValidMove pos player b of
        Just validmove => do
          let updated = move' validmove g
          let _ = !(put (Current . T $ updated))
          pure $ "Great Move: \n" ++ show !get ++ "\n"
        Nothing =>
          pure "Invalid move.\n"



{--

  putStrLn $ "Processing: " ++ s
  let r = (case pparseCommand s of
             Nothing => "Not a valid command: "
             Just ShowGame => show current
             Just HelpGame => "something helpful"
             Just (MoveGame p) => "some move: ")
  putStrLn r
  pure r
--}
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
  OperationSuccess _ <- tcpSend (!(handleCommand (trim str)))
    | RecoverableError err => do putStr ("Error sending: " ++ (show err))
                                 tcpClose
    | FatalError err => do putStr ("Error sending: " ++ (show err))
                           tcpFinalise
    | ConnectionClosed => return ()

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
