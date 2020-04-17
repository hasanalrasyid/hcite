module Hciteservice.Modules.Person.Query
  ( QueryApi
  , querier
  ) where

import           Hciteservice.Modules.Person.Store
import           Hciteservice.Modules.Person.Types
import           Polysemy                              (Members)
import           Servant.API                           ((:>))
import qualified Tendermint.SDK.BaseApp                as BaseApp
import qualified Tendermint.SDK.BaseApp.Store.Map      as M
import Tendermint.SDK.Types.Address (Address (..))

--------------------------------------------------------------------------------
-- | Query API
--------------------------------------------------------------------------------


type QueryApi = "person" :> BaseApp.StoreLeaf (M.Map Address Person)

querier
  :: Members BaseApp.QueryEffs r
  => BaseApp.RouteQ QueryApi r
querier = BaseApp.storeQueryHandler personMap
