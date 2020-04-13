module Hciteservice.Modules.Hciteservice.Store
  ( Name(..)
  , whoisMap
  ) where

import           Control.Lens                          (iso)
import qualified Data.Aeson                            as A
import           Data.Proxy
import           Data.String.Conversions               (cs)
import           Data.Text                             (Text)
import           GHC.TypeLits                          (symbolVal)
import           Hciteservice.Modules.Hciteservice.Types
import qualified Tendermint.SDK.BaseApp                as BaseApp
import qualified Tendermint.SDK.BaseApp.Store.Map      as M

data HciteserviceNamespace

store :: BaseApp.Store HciteserviceNamespace
store = BaseApp.makeStore $
  BaseApp.KeyRoot $ cs . symbolVal $ Proxy @HciteserviceName

newtype Name = Name {unName :: Text} deriving (Eq, Show, A.ToJSON, A.FromJSON)

instance BaseApp.RawKey Name where
    rawKey = iso (\(Name n) -> cs n) (Name . cs)

instance BaseApp.QueryData Name

data WhoisMapKey = WhoisMapKey

instance BaseApp.RawKey WhoisMapKey where
    rawKey = iso (const "whoisMap") (const WhoisMapKey)

instance BaseApp.IsKey WhoisMapKey HciteserviceNamespace where
  type Value WhoisMapKey HciteserviceNamespace = M.Map Name Whois

whoisMap :: M.Map Name Whois
whoisMap = M.makeMap WhoisMapKey store
