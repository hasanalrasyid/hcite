{-# LANGUAGE TemplateHaskell #-}

module Hciteservice.Modules.Person.Keeper
  ( HciteserviceEffs
  , PersonKeeper(..)
  , hciteCoinId
--, setName
--, deleteName
--, buyName
--, faucetAccount
--, getWhois
  , eval
  ) where

import           Hciteservice.Modules.Person.Messages
import           Hciteservice.Modules.Person.Store
import           Hciteservice.Modules.Person.Types
import           Polysemy                                 (Member, Members, Sem,
                                                           interpret, makeSem)
import           Polysemy.Error                           (Error, mapError,
                                                           throw)
import           Polysemy.Output                          (Output)
import qualified Tendermint.SDK.BaseApp                   as BaseApp
import qualified Tendermint.SDK.BaseApp.Store.Map         as M
import           Tendermint.SDK.Modules.Auth              (Coin (..), CoinId)
import           Tendermint.SDK.Modules.Bank              (BankEffs, burn, mint,
                                                           transfer)
import Tendermint.SDK.Types.Address (Address (..))


data PersonKeeper m a where
--  FaucetAccount :: FaucetAccountMsg -> PersonKeeper m ()
  CreatePerson :: CreatePersonMsg -> PersonKeeper m ()
  RetrievePerson :: Address -> PersonKeeper m (Maybe Person)
  UpdatePerson :: UpdatePersonMsg -> PersonKeeper m ()
--  DeletePerson :: DeletePersonMsg -> PersonKeeper m ()
--  SetAddress :: SetNameMsg -> PersonKeeper m ()

makeSem ''PersonKeeper

type HciteserviceEffs = '[PersonKeeper, Error PersonError]

hciteCoinId :: CoinId
hciteCoinId = "hciteservice"

eval
  :: Members BaseApp.TxEffs r
  => Members BankEffs r
  => Members BaseApp.BaseEffs r
  => forall a. Sem (PersonKeeper ': Error PersonError ': r) a
  -> Sem r a
eval = mapError BaseApp.makeAppError . evalHciteservice
  where
    evalHciteservice
      :: Members BaseApp.TxEffs r
      => Members BaseApp.BaseEffs r
      => Members BankEffs r
      => Member (Error PersonError) r
      => Sem (PersonKeeper ': r) a -> Sem r a
    evalHciteservice =
      interpret (\case
--          FaucetAccount msg -> faucetAccountF msg
--          BuyAddress msg -> buyNameF msg
--          DeleteAddress msg -> deleteNameF msg
--          SetAddress msg -> setNameF msg
          CreatePerson msg -> createPersonF msg
          RetrievePerson addr -> getPersonF addr
          UpdatePerson msg -> updatePersonF msg
        )

getPersonF
  :: Members BaseApp.TxEffs r
  => Address -> Sem r (Maybe Person)
getPersonF addr = M.lookup addr personMap

--------------------------------------------------------------------------------
updatePersonF
  :: Members BaseApp.TxEffs r
  => Members BankEffs r
  => Members BaseApp.BaseEffs r
  => Member (Error PersonError) r
  => UpdatePersonMsg
  -> Sem r ()
-- ^ did it succeed
updatePersonF msg = do
  let personAddress = updatePersonOldAddress msg
  let personNewAddress = updatePersonNewAddress msg
  mPerson <- M.lookup personAddress personMap
  case mPerson of
    Nothing -> throw $ InvalidDelete "Can't update non-existant Personnel data."
    Just currentPerson -> do
          M.delete personAddress personMap
          M.insert personAddress (currentPerson {obsoletedBy = Just personNewAddress}) personMap
          M.insert personNewAddress (updatePersonNewDetail msg) personMap
--        BaseApp.emit event
--        BaseApp.logEvent event


createPersonF
  :: Members BaseApp.TxEffs r
  => Members BankEffs r
  => Members BaseApp.BaseEffs r
  => Member (Error PersonError) r
  => CreatePersonMsg
  -> Sem r ()
-- ^ did it succeed
createPersonF msg = do
  let address = createPersonAddress msg
  mPerson <- M.lookup address personMap
  case mPerson of
    Just _ ->throw $ InvalidCreate "Can't recreate existant Person data, try to update"
    Nothing -> createPersonRecord msg
      where
        createPersonRecord
          :: Members BaseApp.TxEffs r
          => Members BaseApp.BaseEffs r
          => Members BankEffs r
          => CreatePersonMsg
          -> Sem r ()
        createPersonRecord CreatePersonMsg{..} = do
          transfer createPersonOperator (Coin hciteCoinId (fromInteger $ toInteger 1)) superAdmin
          if True then return ()
                  else burn createPersonOperator (Coin hciteCoinId (fromInteger $ toInteger 1))
          M.insert address createPersonDetail personMap
          let event = NameClaimed
                { nameClaimedOwner = address
                , nameClaimedName = "Dummy"
                , nameClaimedValue = "Dummy"
                , nameClaimedBid = (fromInteger $ toInteger 1)
                }
          BaseApp.emit event
          BaseApp.logEvent event
  {-
  let name = buyNameAddress msg
  mWhois <- M.lookup (Address name) personMap
  case mWhois of
    -- The name is unclaimed, go ahead and debit the account
    -- and create it.
    Nothing    -> buyUnclaimedAddress msg
    -- The name is currently claimed, we will transfer the
    -- funds and ownership
    Just whois -> buyClaimedAddress msg whois
    where
      buyUnclaimedName
        :: Members BaseApp.TxEffs r
        => Members BaseApp.BaseEffs r
        => Members BankEffs r
        => BuyNameMsg
        -> Sem r ()
      buyUnclaimedAddress BuyNameMsg{..} = do
        burn buyNameBuyer (Coin hciteCoinId buyNameBid)
        let whois = Whois
              { whoisOwner = buyNameBuyer
              , whoisValue = buyNameValue
              , whoisPrice = buyNameBid
              , whoisTitle = buyNameTitle
              , whoisReference = buyNameReference
              }
        M.insert (Address buyNameName) whois whoisMap
        let event = NameClaimed
              { nameClaimedOwner = buyNameBuyer
              , nameClaimedAddress = buyNameName
              , nameClaimedValue = buyNameValue
              , nameClaimedBid = buyNameBid
              }
        BaseApp.emit event
        BaseApp.logEvent event

      buyClaimedName
        :: Members BaseApp.TxEffs r
        => Member (Error PersonError) r
        => Members BaseApp.BaseEffs r
        => Members BankEffs r
        => BuyNameMsg
        -> Whois
        -> Sem r ()
      buyClaimedAddress BuyNameMsg{..} currentWhois =
        let Whois{ whoisPrice = forsalePrice, whoisOwner = previousOwner } = currentWhois
        in if buyNameBid > forsalePrice
             then do
               transfer buyNameBuyer (Coin hciteCoinId buyNameBid) previousOwner
               -- update new owner, price and value based on BuyName
               let whois' = currentWhois
                     { whoisOwner = buyNameBuyer
                     , whoisPrice = buyNameBid
                     , whoisValue = buyNameValue
                     }
               M.insert (Address buyNameName) whois' whoisMap
               let event = NameClaimed
                     { nameClaimedOwner = buyNameBuyer
                     , nameClaimedAddress = buyNameName
                     , nameClaimedValue = buyNameValue
                     , nameClaimedBid = buyNameBid
                     }
               BaseApp.emit event
               BaseApp.logEvent event
             else throw (InsufficientBid "Bid must exceed the price.")
-}
--------------------------------------------------------------------------------

faucetAccountF
  :: Members [BaseApp.Logger, Output BaseApp.Event] r
  => Members BankEffs r
  => FaucetAccountMsg
  -> Sem r ()
faucetAccountF FaucetAccountMsg{..} = do
  let coin = Coin faucetAccountCoinId faucetAccountAmount
  mint faucetAccountTo coin
  let event = Faucetted
        { faucettedAccount = faucetAccountTo
        , faucettedCoinId = faucetAccountCoinId
        , faucettedAmount = faucetAccountAmount
        }
  BaseApp.emit event
  BaseApp.logEvent event

  {-
setNameF
  :: Members BaseApp.TxEffs r
  => Members BaseApp.BaseEffs r
  => Member (Error PersonError) r
  => SetNameMsg
  -> Sem r ()
setNameF SetNameMsg{..} = do
  mwhois <- M.lookup (Address setNameName) whoisMap
  case mwhois of
    Nothing -> throw $ UnauthorizedSet "Cannot claim name with SetMessage tx."
    Just currentWhois@Whois{..} ->
      if whoisOwner /= setNameOwner
        then throw $ UnauthorizedSet "Setter must be the owner of the Name."
        else do
          M.insert (Address setNameName) (currentWhois {whoisValue = setNameValue}) whoisMap
          let event = NameRemapped
                { nameRemappedAddress = setNameName
                , nameRemappedNewValue = setNameValue
                , nameRemappedOldValue = whoisValue
                }
          BaseApp.emit event
          BaseApp.logEvent event

deleteNameF
  :: Members BaseApp.TxEffs r
  => Members BaseApp.BaseEffs r
  => Members BankEffs r
  => Member (Error PersonError) r
  => DeleteNameMsg
  -> Sem r ()
deleteNameF DeleteNameMsg{..} = do
  mWhois <- M.lookup (Address deleteNameName) whoisMap
  case mWhois of
    Nothing -> throw $ InvalidDelete "Can't remove unassigned name."
    Just Whois{..} ->
      if whoisOwner /= deleteNameOwner
        then throw $ InvalidDelete "Deleter must be the owner."
        else do
          mint deleteNameOwner (Coin hciteCoinId whoisPrice)
          M.delete (Address deleteNameName) whoisMap
          let event = NameDeleted
                { nameDeletedAddress = deleteNameName
                }
          BaseApp.emit event
          BaseApp.logEvent event

buyNameF
  :: Members BaseApp.TxEffs r
  => Members BankEffs r
  => Members BaseApp.BaseEffs r
  => Member (Error PersonError) r
  => BuyNameMsg
  -> Sem r ()
-- ^ did it succeed
buyNameF msg = do
  let name = buyNameAddress msg
  mWhois <- M.lookup (Address name) whoisMap
  case mWhois of
    -- The name is unclaimed, go ahead and debit the account
    -- and create it.
    Nothing    -> buyUnclaimedAddress msg
    -- The name is currently claimed, we will transfer the
    -- funds and ownership
    Just whois -> buyClaimedAddress msg whois
    where
      buyUnclaimedName
        :: Members BaseApp.TxEffs r
        => Members BaseApp.BaseEffs r
        => Members BankEffs r
        => BuyNameMsg
        -> Sem r ()
      buyUnclaimedAddress BuyNameMsg{..} = do
        burn buyNameBuyer (Coin hciteCoinId buyNameBid)
        let whois = Whois
              { whoisOwner = buyNameBuyer
              , whoisValue = buyNameValue
              , whoisPrice = buyNameBid
              , whoisTitle = buyNameTitle
              , whoisReference = buyNameReference
              }
        M.insert (Address buyNameName) whois whoisMap
        let event = NameClaimed
              { nameClaimedOwner = buyNameBuyer
              , nameClaimedAddress = buyNameName
              , nameClaimedValue = buyNameValue
              , nameClaimedBid = buyNameBid
              }
        BaseApp.emit event
        BaseApp.logEvent event

      buyClaimedName
        :: Members BaseApp.TxEffs r
        => Member (Error PersonError) r
        => Members BaseApp.BaseEffs r
        => Members BankEffs r
        => BuyNameMsg
        -> Whois
        -> Sem r ()
      buyClaimedAddress BuyNameMsg{..} currentWhois =
        let Whois{ whoisPrice = forsalePrice, whoisOwner = previousOwner } = currentWhois
        in if buyNameBid > forsalePrice
             then do
               transfer buyNameBuyer (Coin hciteCoinId buyNameBid) previousOwner
               -- update new owner, price and value based on BuyName
               let whois' = currentWhois
                     { whoisOwner = buyNameBuyer
                     , whoisPrice = buyNameBid
                     , whoisValue = buyNameValue
                     }
               M.insert (Address buyNameName) whois' whoisMap
               let event = NameClaimed
                     { nameClaimedOwner = buyNameBuyer
                     , nameClaimedAddress = buyNameName
                     , nameClaimedValue = buyNameValue
                     , nameClaimedBid = buyNameBid
                     }
               BaseApp.emit event
               BaseApp.logEvent event
             else throw (InsufficientBid "Bid must exceed the price.")
-}
