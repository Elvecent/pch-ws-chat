{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module MessageHandlers (kick) where

import           Control.Concurrent.STM
import           Data.Map               as M
import           Data.Text              as T
import           Types

kick :: Server -> ClientName -> Text -> STM Text
kick Server{..} name reason = do
  clientMap <- readTVar clients
  case M.lookup name clientMap of
    Nothing -> return $ "User " <> name <> " does not exist."
    Just client -> do
      let k = clientKicked client
      kicked <- readTVar k
      case kicked of
        Just _  -> return $ "User " <> name <> " is already kicked."
        Nothing -> do
          writeTVar k $ Just reason
          return $ "You have kicked  " <> name <> "."
