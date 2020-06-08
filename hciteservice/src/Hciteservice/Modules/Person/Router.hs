module Hciteservice.Modules.Person.Router
  ( MessageApi
  , messageHandlers
  ) where

import           Hciteservice.Modules.Person.Keeper   (HciteserviceEffs,
                                                     createPerson,retrievePerson,
                                                     updatePerson)
import           Hciteservice.Modules.Person.Messages
import           Polysemy                                 (Members, Sem)
import           Servant.API                              ((:<|>) (..))
import           Tendermint.SDK.BaseApp                   ((:~>), BaseEffs,
                                                           Return, RouteTx,
                                                           RoutingTx (..),
                                                           TypedMessage,
                                                           incCount, withTimer)
import           Tendermint.SDK.Types.Message             (Msg (..))
import           Tendermint.SDK.Types.Transaction         (Tx (..))

{-
type MessageApi =
       TypedMessage BuyNameMsg :~> Return ()
  :<|> TypedMessage SetNameMsg :~> Return ()
  :<|> TypedMessage DeleteNameMsg :~> Return ()
  :<|> TypedMessage FaucetAccountMsg :~> Return ()
-}

type MessageApi =
       TypedMessage CreatePersonMsg  :~>  Return ()
  :<|> TypedMessage Address :~>  Return ()
  :<|> TypedMessage UpdatePersonMsg     :~>  Return ()

messageHandlers
  :: Members PersonEffs r
  => Members BaseEffs r
  => RouteTx MessageApi r
messageHandlers = createPersonH :<|> retrievePersonH :<|> updatePersonH
--messageHandlers = buyNameH :<|> setNameH :<|> deleteNameH :<|> faucetH

createPersonH
  :: Members PersonEffs r
  => Members BaseEffs r
  => RoutingTx CreatePersonMsg
  -> Sem r ()
createPersonH (RoutingTx Tx{txMsg=Msg{msgData}}) = do
  incCount "create_total"
  withTimer "create_duration_seconds" $ createPerson msgData

{-
buyNameH
  :: Members PersonEffs r
  => Members BaseEffs r
  => RoutingTx BuyNameMsg
  -> Sem r ()
buyNameH (RoutingTx Tx{txMsg=Msg{msgData}}) = do
  incCount "buy_total"
  withTimer "buy_duration_seconds" $ buyName msgData

setNameH
  :: Members PersonEffs r
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
  -}
