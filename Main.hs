{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.Wai                    (Application)
import           Network.Wai.Handler.Warp       (run)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Network.WebSockets             (Connection, ServerApp,
                                                 acceptRequest,
                                                 defaultConnectionOptions,
                                                 withPingThread)
import           Server                         (newServer, talk)
import           Web.Scotty                     (file, get, scottyApp)

wsApp :: (Connection -> IO ()) -> ServerApp
wsApp serve = \pending -> do
  conn <- acceptRequest pending
  withPingThread conn 30 (return ()) $ do
    putStrLn $ "Accepted connection"
    serve conn

app :: Application -> (Connection -> IO ()) ->  Application
app scApp server =
  websocketsOr
    defaultConnectionOptions
    (wsApp server)
    scApp

main :: IO ()
main = do
  server <- newServer
  scApp <- scottyApp $ get "/" $ file "./index.html"
  run 8080 $ (app scApp) (talk server)
