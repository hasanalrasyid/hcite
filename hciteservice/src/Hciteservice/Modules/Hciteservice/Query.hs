module Hciteservice.Modules.Hciteservice.Query
  ( QueryApi
  , querier
  ) where

import           Hciteservice.Modules.Hciteservice.Store
import           Hciteservice.Modules.Hciteservice.Types
import           Polysemy                              (Members)
import           Servant.API                           ((:>))
import qualified Tendermint.SDK.BaseApp                as BaseApp
import qualified Tendermint.SDK.BaseApp.Store.Map      as M

--------------------------------------------------------------------------------
-- | Query API
--------------------------------------------------------------------------------


type QueryApi = "whois" :> BaseApp.StoreLeaf (M.Map Name Whois)

querier
  :: Members BaseApp.QueryEffs r
  => BaseApp.RouteQ QueryApi r
querier = BaseApp.storeQueryHandler whoisMap
