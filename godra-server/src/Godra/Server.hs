module Godra.Server where

import Hydra.Prelude

import Godra.Game (Move (..), Action (..), Position (..), submitMove)

import Data.Aeson qualified as Aeson
import Control.Exception (IOException)
import Control.Monad.Class.MonadAsync (async)
import Hydra.Cardano.Api (NetworkId (..), NetworkMagic (..))
import Network.HTTP.Types.Status (status200, status400, status404)
import Network.Wai (
  Application,
  pathInfo,
  requestMethod,
  responseFile,
  responseLBS,
 )
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as Wai
import Network.WebSockets qualified as WS
import Hydra.Network (Host (..), readHost)
import Network.WebSockets (Connection, runClient, sendTextData)
import Safe (readMay)
import Prelude (read)


runServer :: IO ()
runServer = do
  key <- requireEnv "GODRA_CARDANO_SIGNING_KEY"
  host <- parseHost =<< requireEnv "HYDRA_API_HOST"
  network <- parseNetwork =<< requireEnv "GODRA_NETWORK"
  port <- read <$> requireEnv "PORT"
  let settings =
        Warp.defaultSettings
          & Warp.setPort port
          & Warp.setHost "0.0.0.0"
          & Warp.setBeforeMainLoop
            ( do
                putStrLn "Server started..."
                putStrLn $ "Listening on: tcp/" <> show port
            )
  Warp.runSettings settings $
    Wai.websocketsOr WS.defaultConnectionOptions (websocketApp host) (httpApp network key host)
 where
  parseHost str =
    case readHost str of
      Nothing -> fail $ "Could not parse host address: " <> str
      Just host -> pure host

  -- Like cardano-cli: "mainnet" or a number for a given testnet network magic.
  parseNetwork str =
    case parseMainnet str <|> parseTestnetMagic str of
      Nothing -> fail $ "Could not parse network id: " <> str <> " (Expected 'mainnet' or a number)"
      Just nid -> pure nid

  parseMainnet str = Mainnet <$ guard (str == "mainnet")

  parseTestnetMagic = fmap (Testnet . NetworkMagic) . readMaybe


-- | Same as 'withClient' except we don't retry if connection fails.
withClientNoRetry :: Host -> (Connection -> IO ()) -> IO ()
withClientNoRetry Host{hostname, port} action =
  runClient (toString hostname) (fromIntegral port) "/" action
    `catch` \(e :: IOException) -> print e >> threadDelay 1


withClient :: Host -> (Connection -> IO ()) -> IO ()
withClient Host{hostname, port} action =
  retry
 where
  retry = do
    putTextLn $ "Connecting to Hydra API on " <> hostname <> ":" <> show port <> ".."
    runClient (toString hostname) (fromIntegral port) "/" action
      `catch` \(e :: IOException) -> print e >> threadDelay 1 >> retry


httpApp :: NetworkId -> FilePath -> Host -> Application
httpApp networkId key host req send = do
  case (requestMethod req, pathInfo req) of
    -- TODO: pass
    ("GET", [ "resign", gameId ] ) -> do
          putStrLn $ "Player resigned in game " <> toString gameId
          -- \| spawn a connection in a new thread
          void $ async $ withClientNoRetry host $ \cnx ->
            submitMove networkId key cnx (Move { gameId = gameId, action = Resign })
          send $ responseLBS status200 corsHeaders "OK"
    ("GET", "move" : gameId : args) -> do
      case traverse (readMay . toString) args of
        Just [x, y] -> do
          putStrLn $ "Player moved at " <> show (x, y) <> " in game " <> toString gameId
          -- \| spawn a connection in a new thread
          void $ async $ withClientNoRetry host $ \cnx ->
            submitMove networkId key cnx (Move { gameId = gameId, action = Play $ Position x y })
          send $ responseLBS status200 corsHeaders "OK"
        _ ->
          send handleError
    _ ->
      send handleNotFound
 where
  handleError = responseLBS status400 corsHeaders "INVALID REQUEST"

  handleNotFound = responseLBS status404 corsHeaders "NOT FOUND"

  handleFile filepath = responseFile status200 corsHeaders filepath Nothing

  corsHeaders =
    [ ("Access-Control-Allow-Origin", "*")
    , ("Access-Control-Allow-Methods", "*")
    , ("Access-Control-Allow-Headers", "*")
    ]


websocketApp :: Host -> WS.PendingConnection -> IO ()
websocketApp host pendingConnection = do
  frontend <- WS.acceptRequest pendingConnection
  withClient host $ \backend ->
    race_
      (forever $ WS.receive frontend >>= WS.send backend)
      (forever $ WS.receive backend >>= WS.send frontend)


-- | Like 'lookupEnv' but terminate program with a message if environment
-- variable is not set.
requireEnv :: String -> IO String
requireEnv name =
  lookupEnv name >>= \case
    Just value -> pure value
    Nothing -> die $ "Error: Required environment variable " <> name <> " not set"

