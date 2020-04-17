module Hciteservice.Modules.Person

  (
    -- * Module
    PersonService
  , personModule
  , module           Hciteservice.Modules.Person.Keeper
  , module           Hciteservice.Modules.Person.Messages
  , module           Hciteservice.Modules.Person.Store
  , module           Hciteservice.Modules.Person.Query
  , module           Hciteservice.Modules.Person.Router
  , module           Hciteservice.Modules.Person.Types


  ) where

import           Data.Proxy
import           Hciteservice.Modules.Person.Keeper
import           Hciteservice.Modules.Person.Messages
import           Hciteservice.Modules.Person.Query
import           Hciteservice.Modules.Person.Router
import           Hciteservice.Modules.Person.Store    (Name (..))
import           Hciteservice.Modules.Person.Types
import           Polysemy                                 (Members)
import           Tendermint.SDK.Application               (Module (..),
                                                           ModuleEffs)
import           Tendermint.SDK.BaseApp                   (EmptyTxServer (..))


type Person =
  Module PersonName MessageApi MessageApi QueryApi HciteserviceEffs '[]

personModule
  :: Members (ModuleEffs Person) r
  => Person r
personModule = Module
  { moduleTxDeliverer = EmptyTxServer
  , moduleTxChecker = EmptyTxServer
  , moduleQuerier = querier
  , moduleEval = eval
  }
