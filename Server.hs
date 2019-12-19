{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Server (talk, newServer, Server) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad

import           Data.Functor             (void)
import           Data.Map
import           Data.Text                (Text)
import qualified Data.Text                as T
import           MessageHandlers
import qualified Network.WebSockets       as WS
import           Types

newClient :: ClientName -> WS.Connection -> STM Client
newClient name conn = do
  c <- newTChan
  k <- newTVar Nothing
  return Client { clientName     = name
                , clientConn     = conn
                , clientKicked   = k
                , clientSendChan = c
                }

newServer :: IO Server
newServer = do
  c <- newTVarIO empty
  return Server { clients = c }

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg =
  writeTChan clientSendChan msg

broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
  clientMap <- readTVar clients
  mapM_
    (\client -> sendMessage client msg) $
    elems clientMap

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server Client{..} message =
  case message of
    Notice msg         -> output $ "*** " <> msg
    Tell name msg      -> output $ "*" <> name <> "*: " <> msg
    Broadcast name msg -> output $ "<" <> name <> ">: " <> msg
    Command msg ->
      case T.unpack <$> T.words msg of
        "/kick" : who : why  -> do
          kickingResult <- atomically $
            kick server (T.pack who) (T.unwords $ T.pack <$> why)
          output kickingResult
        "/quit" : _ ->
          return False
        ('/' : _) : _ -> do
          output $ "Unrecognized command: " <> msg
        _ -> do
          atomically $ broadcast server $ Broadcast clientName msg
          return True
  where
    output :: Text -> IO Bool
    output t = WS.sendTextData clientConn t >> return True

talk :: Server -> WS.Connection -> IO ()
talk server@Server{..} conn = do
  send "What is your name?"
  name <- WS.receiveData conn
  if T.null name
    then talk server conn
    else
    do
      ok <- checkAddClient server name conn
      case ok of
        Nothing -> do
          send "This name is already in use."
          talk server conn
        Just client ->
          runClient server client `finally` removeClient server name
  where
    send :: Text -> IO ()
    send = WS.sendTextData conn

runClient :: Server -> Client -> IO ()
runClient server@Server{..} client@Client{..} = do
  void $ race serve receive
  where
    receive = forever $ do
      msg <- WS.receiveData clientConn
      atomically $ sendMessage client (Command msg)

    serve = join . atomically $ do
      k <- readTVar clientKicked
      case k of
        Just reason -> return $
          WS.sendTextData clientConn $
          ("You have been kicked: " :: Text) <> reason
        Nothing -> do
          msg <- readTChan clientSendChan
          return $ do
            continue <- handleMessage server client msg
            when continue $ serve

checkAddClient :: Server -> ClientName -> WS.Connection -> IO (Maybe Client)
checkAddClient server@Server{..} name conn = atomically $ do
  clientMap <- readTVar clients
  if member name clientMap
    then return Nothing
    else do client <- newClient name conn
            writeTVar clients $ insert name client clientMap
            broadcast server  $ Notice (name <> " has connected")
            return $ Just client

removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
  modifyTVar' clients $ delete name
  broadcast server $ Notice (name <> " left")
