module Hciteservice.Modules.Hciteservice

  (
    -- * Module
    Hciteservice
  , hciteModule
  , module           Hciteservice.Modules.Hciteservice.Keeper
  , module           Hciteservice.Modules.Hciteservice.Messages
  , module           Hciteservice.Modules.Hciteservice.Store
  , module           Hciteservice.Modules.Hciteservice.Query
  , module           Hciteservice.Modules.Hciteservice.Router
  , module           Hciteservice.Modules.Hciteservice.Types


  ) where

import           Data.Proxy
import           Hciteservice.Modules.Hciteservice.Keeper
import           Hciteservice.Modules.Hciteservice.Messages
import           Hciteservice.Modules.Hciteservice.Query
import           Hciteservice.Modules.Hciteservice.Router
import           Hciteservice.Modules.Hciteservice.Store    (Name (..))
import           Hciteservice.Modules.Hciteservice.Types
import           Polysemy                                 (Members)
import           Tendermint.SDK.Application               (Module (..),
                                                           ModuleEffs)
import           Tendermint.SDK.BaseApp                   (DefaultCheckTx (..))
import           Tendermint.SDK.Modules.Bank              (Bank)


type Hciteservice =
  Module HciteserviceName MessageApi MessageApi QueryApi HciteserviceEffs '[Bank]

hciteModule
  :: Members (ModuleEffs Hciteservice) r
  => Hciteservice r
hciteModule = Module
  { moduleTxDeliverer = messageHandlers
  , moduleTxChecker = defaultCheckTx (Proxy :: Proxy MessageApi) (Proxy :: Proxy r)
  , moduleQuerier = querier
  , moduleEval = eval
  }
