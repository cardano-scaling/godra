module Godra.Game where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Control.Exception (IOException)
import Data.Aeson qualified as Aeson
import Hydra.API.ClientInput (ClientInput (GetUTxO, NewTx))
import Hydra.API.ServerOutput (ServerOutput (GetUTxOResponse))
import Hydra.Chain.Direct.State ()
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.Network (Host (..))
import Network.WebSockets (Connection, runClient, sendTextData)
import Network.WebSockets.Connection (receive, receiveData)


data Move = Move
  { gameId :: Text
  , action :: Action
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data Action
  = Play Position
  | Pass
  | Resign
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data Position = Position
  { x :: Int
  , y :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Sends a move to Hydra
submitMove
  :: NetworkId
  -> FilePath
  -> Connection
  -> Move
  -> IO ()
submitMove networkId signingKeyPath cnx move = do
  sk <- readFileTextEnvelopeThrow (AsSigningKey AsPaymentKey) signingKeyPath
  let myAddress = mkVkAddress networkId $ getVerificationKey sk
  flushQueue
  sendTextData @Text cnx $ decodeUtf8 $ Aeson.encode (GetUTxO @Tx)
  msg <- receiveData cnx
  putStrLn $ "Received from hydra-node: " <> show msg
  case Aeson.eitherDecode @(ServerOutput Tx) msg of
    Right (GetUTxOResponse _ utxo) ->
      case UTxO.find (\TxOut{txOutAddress} -> txOutAddress == myAddress) utxo of
        Nothing -> fail $ "No UTxO owned by " <> show myAddress
        Just (txIn, txOut) ->
          case mkMoveTx (txIn, txOut) sk move of
            Right tx -> sendTextData cnx $ Aeson.encode $ NewTx tx
            Left err -> fail $ "Failed to build move transaction " <> show err
    Right _ -> fail $ "Unexpected server answer:  " <> decodeUtf8 msg
    Left e -> fail $ "Failed to decode server answer:  " <> show e
 where
  flushQueue =
    race_ (threadDelay 0.25) (void (receive cnx) >> flushQueue)


-- | Create a zero-fee, payment cardano transaction with move metadata, which
-- just re-spends the given UTxO.
mkMoveTx ::
  -- | UTxO to spend
  (TxIn, TxOut CtxUTxO) ->
  -- | Signing key which owns the UTxO.
  SigningKey PaymentKey ->
  -- | Move data
  Move ->
  Either TxBodyError Tx
mkMoveTx (txin, txOut) sk move = do
  body <- createAndValidateTransactionBody bodyContent
  pure $ signShelleyTransaction body [WitnessPaymentKey sk]
 where
  bodyContent =
    defaultTxBodyContent
      & addTxIn (txin, BuildTxWith $ KeyWitness KeyWitnessForSpending)
      & addTxOut (toTxContext txOut)
      & setTxFee (TxFeeExplicit $ Coin 0)
      & setTxMetadata metadata

  -- Note: g = 7, o = 15 => 715 = "go".
  key = 715
  metadata = TxMetadataInEra $ TxMetadata $ fromList [(key, metaValue)]

  -- XX: Hack
  Right metaValue = metadataValueFromJsonNoSchema (Aeson.toJSON move)
