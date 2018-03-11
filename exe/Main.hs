
module Main where

import Protolude hiding (option)

import Config (Config(..), readConfig)
import Nanocoin (initNode)
import Nanocoin.Network.Utils (RPCPort, P2PPort)

import Options.Applicative
import Network.Socket (HostName, PortNumber)

fallback :: Parser (Maybe a) -> a -> Parser a
fallback parser x = fallback' x <$> parser
  where
    fallback' (Just x) _ = x
    fallback' Nothing x  = x

main :: IO ()
main = do
    -- Read config from config file
    defaultConfig <- readConfig "config"
    -- Override config with cmd line arguments
    config <- execParser $
      info (parseConfig defaultConfig) mempty
    initNode config
  where
    parseConfig config = Config
      <$> hostnameParser `fallback` (hostname config)
      <*> rpcPortParser  `fallback` (rpcPort config)
      <*> p2pPortParser  `fallback` (p2pPort config)
      <*> pure (bootnodes config) -- XXX add cmd line option for adding bootnodes
      <*> keysParser
      <*> logFileParser

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
