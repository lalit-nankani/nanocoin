
module Config where

import Protolude hiding (option)

import Options.Applicative

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

import Network.Socket (HostName, PortNumber)

import Nanocoin.Network.Utils (RPCPort, P2PPort)

data Config = Config
  { hostname     :: HostName        -- ^ Hostname of node
  , rpcPort      :: RPCPort         -- ^ Port to run RPC server on
  , p2pPort      :: P2PPort         -- ^ Port to run p2p process on
  , bootnodes    :: [Text]          -- ^ List of bootnode hostnames and ports
  , keysFilePath :: Maybe FilePath  -- ^ Filepath to read node keys from
  , logFilePath  :: Maybe FilePath  -- ^ Filepath to log to
  }

-- Read config from config file
readConfig :: FilePath -> IO Config
readConfig cfgFile = do
  cfg <- C.load [C.Required cfgFile]
  Config <$> C.require cfg "nanocoin.hostname"
         <*> C.require cfg "nanocoin.rpcPort"
         <*> readP2PPort cfg
         <*> C.require cfg "nanocoin.bootnodes"
         <*> C.lookup  cfg "nanocoin.keysPath"
         <*> C.lookup  cfg "nanocoin.logFilePath"
  where
    readP2PPort :: C.Config -> IO P2PPort
    readP2PPort cfg = do
      portNum <- C.require cfg "nanocoin.p2pPort"
      return $ intToP2PPort portNum

    intToP2PPort :: Int -> P2PPort
    intToP2PPort = fromIntegral

-- Parse Config options via command line
parseConfig config = Config
    <$> hostnameParser `fallback` (hostname config)
    <*> rpcPortParser  `fallback` (rpcPort config)
    <*> p2pPortParser  `fallback` (p2pPort config)
    <*> pure (bootnodes config) -- XXX add cmd line option for adding bootnodes
    <*> keysParser
    <*> logFileParser
  where
    hostnameParser :: Parser (Maybe HostName)
    hostnameParser = optional $
      option auto $ long "hostname"
                 <> metavar "HOSTNAME"

    rpcPortParser :: Parser (Maybe RPCPort)
    rpcPortParser = optional $
      option auto $ long "rpc-port"
                 <> short 'p'
                 <> metavar "RPC_PORT"

    p2pPortParser :: Parser (Maybe P2PPort)
    p2pPortParser =
      fmap (fmap fromIntegral) $
        optional $ option auto $
             long "p2p-port"
          <> short 'n'
          <> metavar "P2P_PORT"

    keysParser :: Parser (Maybe FilePath)
    keysParser = optional $
      strOption $ long "keys"
               <> short 'k'
               <> metavar "KEYS_DIR"

    logFileParser :: Parser (Maybe FilePath)
    logFileParser = optional $
      strOption $ long "logfile"
              <> short 'f'
              <> metavar "LOG_FILE"

fallback :: Parser (Maybe a) -> a -> Parser a
fallback parser x = flip fallback' x <$> parser
  where
    fallback' (Just x) _ = x
    fallback' Nothing x  = x
