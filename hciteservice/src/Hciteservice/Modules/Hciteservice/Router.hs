module Hciteservice.Modules.Hciteservice.Router
  ( MessageApi
  , messageHandlers
  ) where

import           Hciteservice.Modules.Hciteservice.Keeper   (HciteserviceEffs,
                                                           buyName, deleteName,
                                                           faucetAccount,
                                                           setName)
import           Hciteservice.Modules.Hciteservice.Messages
import           Polysemy                                 (Members, Sem)
import           Servant.API                              ((:<|>) (..))
import           Tendermint.SDK.BaseApp                   ((:~>), BaseEffs,
                                                           Return, RouteTx,
                                                           RoutingTx (..),
                                                           TypedMessage,
                                                           incCount, withTimer)
import           Tendermint.SDK.Types.Message             (Msg (..))
import           Tendermint.SDK.Types.Transaction         (Tx (..))


type MessageApi =
       TypedMessage BuyNameMsg :~> Return ()
  :<|> TypedMessage SetNameMsg :~> Return ()
  :<|> TypedMessage DeleteNameMsg :~> Return ()
  :<|> TypedMessage FaucetAccountMsg :~> Return ()

messageHandlers
  :: Members HciteserviceEffs r
  => Members BaseEffs r
  => RouteTx MessageApi r
messageHandlers = buyNameH :<|> setNameH :<|> deleteNameH :<|> faucetH

buyNameH
  :: Members HciteserviceEffs r
  => Members BaseEffs r
  => RoutingTx BuyNameMsg
  -> Sem r ()
buyNameH (RoutingTx Tx{txMsg=Msg{msgData}}) = do
  incCount "buy_total"
  withTimer "buy_duration_seconds" $ buyName msgData

setNameH
  :: Members HciteserviceEffs r
  => Members BaseEffs r
  => RoutingTx SetNameMsg
  -> Sem r ()
setNameH (RoutingTx Tx{txMsg=Msg{msgData}}) = do
  incCount "set_total"
  withTimer "set_duration_seconds" $ setName msgData

deleteNameH
  :: Members HciteserviceEffs r
  => Members BaseEffs r
  => RoutingTx DeleteNameMsg
  -> Sem r ()
deleteNameH (RoutingTx Tx{txMsg=Msg{msgData}}) = do
  incCount "delete_total"
  withTimer "delete_duration_seconds" $ deleteName msgData

faucetH
  :: Members HciteserviceEffs r
  => RoutingTx FaucetAccountMsg
  -> Sem r ()
faucetH (RoutingTx Tx{txMsg=Msg{msgData}}) =
  faucetAccount msgData
