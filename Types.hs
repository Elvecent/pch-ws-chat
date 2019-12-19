module Types
  ( ClientName
  , Client(..)
  , Message(..)
  , Server(..)
  ) where

import           Control.Concurrent.STM (TChan, TVar)
import           Data.Map               (Map)
import           Data.Text              (Text)
import           Network.WebSockets     (Connection)

type ClientName = Text
data Client = Client
  { clientName     :: ClientName
  , clientConn     :: Connection
  , clientKicked   :: TVar (Maybe Text)
  , clientSendChan :: TChan Message
  }

data Message
  = Notice Text
  | Tell ClientName Text
  | Broadcast ClientName Text
  | Command Text

data Server = Server
  { clients :: TVar (Map ClientName Client)
  }
