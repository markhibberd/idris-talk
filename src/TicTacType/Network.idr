module TicTacType.Network

import Effect.StdIO
import Effect.System
import Effect.State

import IdrisNet.Socket
import IdrisNet.TCP.TCPServer

--import TicTacType.Data
--import TicTacType.Effect

--%default total

%logging 0

receive' : { [STDIO, STATE Nat, TCPSERVERCLIENT ClientConnected] ==>
             [STDIO, STATE Nat, TCPSERVERCLIENT ()] } Eff IO ()
receive' = do
  -- Receive
  putStrLn "Waiting...."
  current <- get
  OperationSuccess (str, len) <- tcpRecv 1024
    | RecoverableError _ => receive'
    | FatalError err => do putStr ("Error receiving: " ++ (show err))
                           tcpFinalise
    | ConnectionClosed => return ()
  -- Echo
  OperationSuccess _ <- tcpSend str
    | RecoverableError err => do putStr ("Error sending: " ++ (show err))
                                 tcpClose
    | FatalError err => do putStr ("Error sending: " ++ (show err))
                           tcpFinalise
    | ConnectionClosed => return ()
  put (S current)
  putStrLn $ "You have received " ++ show !get ++ " messages"
  receive'

receive : ClientProgram ()
receive = new (new receive')

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


go : IO ()
go = run (setupServer 1235 False)
