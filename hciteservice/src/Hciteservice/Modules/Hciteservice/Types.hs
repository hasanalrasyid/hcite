{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hciteservice.Modules.Hciteservice.Types where

import           Data.Aeson                   as A
import           Data.Bifunctor               (bimap)
import           Data.String.Conversions      (cs)
import           Data.Text                    (Text)
import           Data.Text.Lazy               (fromStrict,toStrict)
import           Data.Word                    (Word64)
import           GHC.Generics                 (Generic)
import           Hciteservice.Aeson            (defaultHciteserviceOptions)
import           Proto3.Suite  {-               (Message, Named, fromByteString,
                                               toLazyByteString)
-}
import qualified Tendermint.SDK.BaseApp       as BaseApp
import           Tendermint.SDK.Codec         (HasCodec (..))
import           Tendermint.SDK.Modules.Auth  (Amount (..), CoinId (..))
import           Tendermint.SDK.Modules.Bank  ()
import           Tendermint.SDK.Types.Address (Address)

import qualified Model as M
import qualified Proto3.Wire.Encode as Encode
import qualified Proto3.Wire.Decode as Decode
import Data.Maybe

--------------------------------------------------------------------------------

type HciteserviceName = "hciteservice"

type Reference = M.Reference

instance MessageField Reference where
  encodeMessageField num x
    | isDefault x = mempty
    | otherwise = encodePrimitive num x


-- need to be updated for complete case of Reference
instance Primitive Reference where
  --encodePrimitive num x = Encode.int64 num $ fromIntegral $ M.referenceSerial x
  encodePrimitive num x = Encode.text num $ fromStrict $ fromMaybe "" $ M.referenceAbstract x
  decodePrimitive = fmap toReference Decode.text
    where toReference x = def {M.referenceAbstract = Just $ toStrict x}

instance HasDefault Reference where
  def = def

instance Named Reference where
--------------------------------------------------------------------------------

data Whois = Whois
  { whoisValue :: Text
  , whoisOwner :: Address
  , whoisPrice :: Amount
  , whoisReference :: Reference
  } deriving (Eq, Show)

data WhoisMessage = WhoisMessage
  { whoisMessageValue :: Text
  , whoisMessageOwner :: Address
  , whoisMessagePrice :: Word64
  } deriving (Eq, Show, Generic)
instance Message WhoisMessage
instance Named WhoisMessage

instance HasCodec Whois where
  encode Whois {..} =
    let whoisMessage = WhoisMessage
          { whoisMessageValue = whoisValue
          , whoisMessageOwner = whoisOwner
          , whoisMessagePrice = unAmount whoisPrice
          }
    in cs . toLazyByteString $ whoisMessage
  decode =
    let toWhois WhoisMessage {..} = Whois
          { whoisValue = whoisMessageValue
          , whoisOwner = whoisMessageOwner
          , whoisPrice = Amount whoisMessagePrice
          , whoisReference = def
          }
    in bimap (cs . show) toWhois . fromByteString @WhoisMessage

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data HciteserviceError =
    InsufficientBid Text
  | UnauthorizedSet Text
  | InvalidDelete Text

instance BaseApp.IsAppError HciteserviceError where
  makeAppError (InsufficientBid msg) =
    BaseApp.AppError
      { appErrorCode = 1
      , appErrorCodespace = "hciteservice"
      , appErrorMessage = msg
      }
  makeAppError (UnauthorizedSet msg) =
    BaseApp.AppError
      { appErrorCode = 2
      , appErrorCodespace = "hciteservice"
      , appErrorMessage = msg
      }
  makeAppError (InvalidDelete msg) =
    BaseApp.AppError
      { appErrorCode = 3
      , appErrorCodespace = "hciteservice"
      , appErrorMessage = msg
      }

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

data Faucetted = Faucetted
  { faucettedAccount :: Address
  , faucettedCoinId  :: CoinId
  , faucettedAmount  :: Amount
  } deriving (Eq, Show, Generic)

faucettedAesonOptions :: A.Options
faucettedAesonOptions = defaultHciteserviceOptions "faucetted"

instance ToJSON Faucetted where
  toJSON = A.genericToJSON faucettedAesonOptions
instance FromJSON Faucetted where
  parseJSON = A.genericParseJSON faucettedAesonOptions
instance BaseApp.ToEvent Faucetted
instance BaseApp.Select Faucetted

data NameClaimed = NameClaimed
  { nameClaimedOwner :: Address
  , nameClaimedName  :: Text
  , nameClaimedValue :: Text
  , nameClaimedBid   :: Amount
  } deriving (Eq, Show, Generic)

nameClaimedAesonOptions :: A.Options
nameClaimedAesonOptions = defaultHciteserviceOptions "nameClaimed"

instance ToJSON NameClaimed where
  toJSON = A.genericToJSON nameClaimedAesonOptions
instance FromJSON NameClaimed where
  parseJSON = A.genericParseJSON nameClaimedAesonOptions
instance BaseApp.ToEvent NameClaimed
instance BaseApp.Select NameClaimed

data NameRemapped = NameRemapped
  { nameRemappedName     :: Text
  , nameRemappedOldValue :: Text
  , nameRemappedNewValue :: Text
  } deriving (Eq, Show, Generic)

nameRemappedAesonOptions :: A.Options
nameRemappedAesonOptions = defaultHciteserviceOptions "nameRemapped"

instance ToJSON NameRemapped where
  toJSON = A.genericToJSON nameRemappedAesonOptions
instance FromJSON NameRemapped where
  parseJSON = A.genericParseJSON nameRemappedAesonOptions
instance BaseApp.ToEvent NameRemapped
instance BaseApp.Select NameRemapped

data NameDeleted = NameDeleted
  { nameDeletedName :: Text
  } deriving (Eq, Show, Generic)

nameDeletedAesonOptions :: A.Options
nameDeletedAesonOptions = defaultHciteserviceOptions "nameDeleted"

instance ToJSON NameDeleted where
  toJSON = A.genericToJSON nameDeletedAesonOptions
instance FromJSON NameDeleted where
  parseJSON = A.genericParseJSON nameDeletedAesonOptions
instance BaseApp.ToEvent NameDeleted
instance BaseApp.Select NameDeleted
