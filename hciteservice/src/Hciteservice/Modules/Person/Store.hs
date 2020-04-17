module Hciteservice.Modules.Person.Store
  ( personMap
  ) where

import           Control.Lens                          (iso)
import           Data.Proxy
import           Data.String.Conversions               (cs)
import           GHC.TypeLits                          (symbolVal)
import           Hciteservice.Modules.Person.Types
import qualified Tendermint.SDK.BaseApp                as BaseApp
import qualified Tendermint.SDK.BaseApp.Store.Map      as M
import Tendermint.SDK.Types.Address (Address (..))

data PersonNamespace

store :: BaseApp.Store PersonNamespace
store = BaseApp.makeStore $
  BaseApp.KeyRoot $ cs . symbolVal $ Proxy @PersonName

--newtype AddressPerson = AddressPerson {unAP :: Address} deriving (Eq, Show, A.ToJSON, A.FromJSON)
--instance BaseApp.RawKey Address where
--    rawKey = iso (\(Address n) -> cs n) (Address . cs)

--instance BaseApp.QueryData Address

data PersonMapKey = PersonMapKey

instance BaseApp.RawKey PersonMapKey where
    rawKey = iso (const "personMap") (const PersonMapKey)

instance BaseApp.IsKey PersonMapKey PersonNamespace where
  type Value PersonMapKey PersonNamespace = M.Map Address Person

personMap :: M.Map Address Person
personMap = M.makeMap PersonMapKey store
