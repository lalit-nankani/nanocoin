
module Config where

import Protolude

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Network.Socket (HostName, PortNumber)

import Nanocoin.Network.Utils (RPCPort, P2PPort)

type BootnodeConfig = (HostName, PortNumber)

data Config = Config
  { hostname     :: HostName         -- ^ Hostname of node
  , rpcPort      :: RPCPort          -- ^ Port to run RPC server on
  , p2pPort      :: P2PPort          -- ^ Port to run p2p process on
  , bootnodes    :: [BootnodeConfig] -- ^ List of bootnode hostnames and ports
  , keysFilePath :: Maybe FilePath   -- ^ Filepath to read node keys from
  , logFilePath  :: Maybe FilePath   -- ^ Filepath to log to
  }

readConfig :: FilePath -> IO Config
readConfig cfgFile = do
  cfg <- C.load [C.Required cfgFile]
  Config <$> C.require cfg "nanocoin.hostname"
         <*> C.require cfg "nanocoin.rpcPort"
         <*> readP2PPort cfg
         <*> readBootnodes cfg
         <*> C.lookup  cfg "nanocoin.keysPath"
         <*> C.lookup  cfg "nanocoin.logFilePath"
  where
    readP2PPort :: C.Config -> IO P2PPort
    readP2PPort cfg = do
      portNum <- C.require cfg "nanocoin.p2pPort"
      return $ intToP2PPort portNum

    readBootnodes :: C.Config -> IO [BootnodeConfig]
    readBootnodes cfg = do
      bnCfg <- C.require cfg "nanocoin.bootnodes"
      return $ map (second intToP2PPort) bnCfg

    intToP2PPort :: Int -> P2PPort
    intToP2PPort = fromIntegral
