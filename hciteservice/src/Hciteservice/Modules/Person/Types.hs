{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hciteservice.Modules.Person.Types where

import           Data.Aeson                   as A
import           Data.Bifunctor               (bimap)
import           Data.String.Conversions      (cs)
import           Data.Text                    (Text)
import           Data.Word                    (Word64)
import           GHC.Generics                 (Generic)
import           Hciteservice.Aeson            (defaultHciteserviceOptions)
import           Proto3.Suite                 (Message, Named, fromByteString,
                                               toLazyByteString)
import qualified Tendermint.SDK.BaseApp       as BaseApp
import           Tendermint.SDK.Codec         (HasCodec (..))
import           Tendermint.SDK.Modules.Auth  (Amount (..), CoinId (..))
import           Tendermint.SDK.Modules.Bank  ()
import           Tendermint.SDK.Types.Address

import Proto3.Suite.Class
import Proto3.Wire.Decode as Decode
import Data.ByteString (ByteString)
import qualified Model as M
--------------------------------------------------------------------------------

type PersonName = "person"
type HCiteReference = M.Reference
--------------------------------------------------------------------------------

superAdmin :: Address
superAdmin = Address "0xaaaaaaaaaaf141fa887c8abc3b4c5b4b9854d5d20d8d15adfffd9e7cfddde8bc"

data Person = Person
  { obsoletedBy :: Maybe Address
  , unPerson :: M.Personnel} deriving (Eq,Show,Generic)

data Whois = Whois
  { whoisValue :: Text
  , whoisOwner :: Address
  , whoisPrice :: Amount
  , whoisTitle :: Text
  , whoisReference :: HCiteReference
  } deriving (Eq, Show)

instance MessageField HCiteReference
instance HasDefault HCiteReference
instance Named HCiteReference
instance Primitive HCiteReference where
  encodePrimitive n r = encodePrimitive n $ M.referenceTitle r
  decodePrimitive = referenceFromBytes <$> Decode.byteString

referenceFromBytes
  :: ByteString
  -> HCiteReference
referenceFromBytes _ = def

instance MessageField Person
instance HasDefault Person
instance HasDefault M.Pegawai where
  def = def
  isDefault a = 0 == M.pegawaiKodeAbsen a

instance Primitive Person where
  encodePrimitive n l = encodePrimitive n $ M.pegawaiNama $ unPerson l
  decodePrimitive = personnelFromBytes <$> Decode.byteString

personnelFromBytes
  :: ByteString
  -> Person
personnelFromBytes _ = def

instance Named Person

data PersonMessage = PersonMessage
  { personMessageObsoletedBy :: Maybe Address
  , personMessageValue :: M.Personnel
  } deriving (Eq, Show, Generic)
instance Message PersonMessage
instance Named PersonMessage

instance MessageField (Maybe Address)
instance Primitive (Maybe Address) where
  encodePrimitive n Nothing = encodePrimitive n ("" :: ByteString)
  encodePrimitive n (Just addr) = encodePrimitive n addr
  decodePrimitive = mAddressFromBytes <$> Decode.byteString

mAddressFromBytes
  :: ByteString
  -> Maybe Address
mAddressFromBytes "" = Nothing
mAddressFromBytes x = Just $ addressFromBytes x

instance Named (Maybe Address)

instance MessageField M.Pegawai
instance Primitive M.Pegawai where
  encodePrimitive n r = encodePrimitive n $ M.pegawaiNama r
  decodePrimitive = pegawaiFromBytes <$> Decode.byteString

pegawaiFromBytes
  :: ByteString
  -> M.Pegawai
pegawaiFromBytes _ = def

instance Named M.Pegawai

data WhoisMessage = WhoisMessage
  { whoisMessageValue :: Text
  , whoisMessageOwner :: Address
  , whoisMessagePrice :: Word64
  , whoisMessageTitle :: Text
  , whoisMessageReference :: M.Reference
  } deriving (Eq, Show, Generic)
instance Message WhoisMessage
instance Named WhoisMessage


instance HasCodec Person where
  encode Person {..} =
    let personMessage = PersonMessage
            { personMessageObsoletedBy = obsoletedBy
            , personMessageValue = unPerson
            }
    in cs . toLazyByteString $ personMessage
  decode =
    let toPerson PersonMessage {..} = Person personMessageObsoletedBy personMessageValue
    in bimap (cs . show) toPerson . fromByteString @PersonMessage


instance HasCodec Whois where
  encode Whois {..} =
    let whoisMessage = WhoisMessage
          { whoisMessageValue = whoisValue
          , whoisMessageOwner = whoisOwner
          , whoisMessagePrice = unAmount whoisPrice
          , whoisMessageTitle = whoisTitle
          , whoisMessageReference = whoisReference
          }
    in cs . toLazyByteString $ whoisMessage
  decode =
    let toWhois WhoisMessage {..} = Whois
          { whoisValue = whoisMessageValue
          , whoisOwner = whoisMessageOwner
          , whoisPrice = Amount whoisMessagePrice
          , whoisTitle = whoisMessageTitle
          , whoisReference = whoisMessageReference
          }
    in bimap (cs . show) toWhois . fromByteString @WhoisMessage

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data PersonError =
    InsufficientBid Text
  | UnauthorizedSet Text
  | InvalidDelete Text
  | InvalidCreate Text

instance BaseApp.IsAppError PersonError where
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
  makeAppError (InvalidCreate msg) =
    BaseApp.AppError
      { appErrorCode = 4
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
