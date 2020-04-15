{-# LANGUAGE TemplateHaskell #-}

module Hciteservice.Modules.Hciteservice.Keeper
  ( HciteserviceEffs
  , HciteserviceKeeper(..)
  , hciteCoinId
  , setName
  , deleteName
  , buyName
  , faucetAccount
  , getWhois
  , eval
  ) where

import           Hciteservice.Modules.Hciteservice.Messages
import           Hciteservice.Modules.Hciteservice.Store
import           Hciteservice.Modules.Hciteservice.Types
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

data HciteserviceKeeper m a where
  FaucetAccount :: FaucetAccountMsg -> HciteserviceKeeper m ()
  BuyName :: BuyNameMsg -> HciteserviceKeeper m ()
  DeleteName :: DeleteNameMsg -> HciteserviceKeeper m ()
  SetName :: SetNameMsg -> HciteserviceKeeper m ()
  GetWhois :: Name -> HciteserviceKeeper m (Maybe Whois)

makeSem ''HciteserviceKeeper

type HciteserviceEffs = '[HciteserviceKeeper, Error HciteserviceError]

hciteCoinId :: CoinId
hciteCoinId = "hciteservice"

eval
  :: Members BaseApp.TxEffs r
  => Members BankEffs r
  => Members BaseApp.BaseEffs r
  => forall a. Sem (HciteserviceKeeper ': Error HciteserviceError ': r) a
  -> Sem r a
eval = mapError BaseApp.makeAppError . evalHciteservice
  where
    evalHciteservice
      :: Members BaseApp.TxEffs r
      => Members BaseApp.BaseEffs r
      => Members BankEffs r
      => Member (Error HciteserviceError) r
      => Sem (HciteserviceKeeper ': r) a -> Sem r a
    evalHciteservice =
      interpret (\case
          FaucetAccount msg -> faucetAccountF msg
          BuyName msg -> buyNameF msg
          DeleteName msg -> deleteNameF msg
          SetName msg -> setNameF msg
          GetWhois name -> getWhoisF name
        )

getWhoisF
  :: Members BaseApp.TxEffs r
  => Name -> Sem r (Maybe Whois)
getWhoisF name = M.lookup name whoisMap

--------------------------------------------------------------------------------

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

setNameF
  :: Members BaseApp.TxEffs r
  => Members BaseApp.BaseEffs r
  => Member (Error HciteserviceError) r
  => SetNameMsg
  -> Sem r ()
setNameF SetNameMsg{..} = do
  mwhois <- M.lookup (Name setNameName) whoisMap
  case mwhois of
    Nothing -> throw $ UnauthorizedSet "Cannot claim name with SetMessage tx."
    Just currentWhois@Whois{..} ->
      if whoisOwner /= setNameOwner
        then throw $ UnauthorizedSet "Setter must be the owner of the Name."
        else do
          M.insert (Name setNameName) (currentWhois {whoisValue = setNameValue}) whoisMap
          let event = NameRemapped
                { nameRemappedName = setNameName
                , nameRemappedNewValue = setNameValue
                , nameRemappedOldValue = whoisValue
                }
          BaseApp.emit event
          BaseApp.logEvent event

deleteNameF
  :: Members BaseApp.TxEffs r
  => Members BaseApp.BaseEffs r
  => Members BankEffs r
  => Member (Error HciteserviceError) r
  => DeleteNameMsg
  -> Sem r ()
deleteNameF DeleteNameMsg{..} = do
  mWhois <- M.lookup (Name deleteNameName) whoisMap
  case mWhois of
    Nothing -> throw $ InvalidDelete "Can't remove unassigned name."
    Just Whois{..} ->
      if whoisOwner /= deleteNameOwner
        then throw $ InvalidDelete "Deleter must be the owner."
        else do
          mint deleteNameOwner (Coin hciteCoinId whoisPrice)
          M.delete (Name deleteNameName) whoisMap
          let event = NameDeleted
                { nameDeletedName = deleteNameName
                }
          BaseApp.emit event
          BaseApp.logEvent event

buyNameF
  :: Members BaseApp.TxEffs r
  => Members BankEffs r
  => Members BaseApp.BaseEffs r
  => Member (Error HciteserviceError) r
  => BuyNameMsg
  -> Sem r ()
-- ^ did it succeed
buyNameF msg = do
  let name = buyNameName msg
  mWhois <- M.lookup (Name name) whoisMap
  case mWhois of
    -- The name is unclaimed, go ahead and debit the account
    -- and create it.
    Nothing    -> buyUnclaimedName msg
    -- The name is currently claimed, we will transfer the
    -- funds and ownership
    Just whois -> buyClaimedName msg whois
    where
      buyUnclaimedName
        :: Members BaseApp.TxEffs r
        => Members BaseApp.BaseEffs r
        => Members BankEffs r
        => BuyNameMsg
        -> Sem r ()
      buyUnclaimedName BuyNameMsg{..} = do
        burn buyNameBuyer (Coin hciteCoinId buyNameBid)
        let whois = Whois
              { whoisOwner = buyNameBuyer
              , whoisValue = buyNameValue
              , whoisPrice = buyNameBid
              , whoisTitle = buyNameTitle
              }
        M.insert (Name buyNameName) whois whoisMap
        let event = NameClaimed
              { nameClaimedOwner = buyNameBuyer
              , nameClaimedName = buyNameName
              , nameClaimedValue = buyNameValue
              , nameClaimedBid = buyNameBid
              }
        BaseApp.emit event
        BaseApp.logEvent event

      buyClaimedName
        :: Members BaseApp.TxEffs r
        => Member (Error HciteserviceError) r
        => Members BaseApp.BaseEffs r
        => Members BankEffs r
        => BuyNameMsg
        -> Whois
        -> Sem r ()
      buyClaimedName BuyNameMsg{..} currentWhois =
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
               M.insert (Name buyNameName) whois' whoisMap
               let event = NameClaimed
                     { nameClaimedOwner = buyNameBuyer
                     , nameClaimedName = buyNameName
                     , nameClaimedValue = buyNameValue
                     , nameClaimedBid = buyNameBid
                     }
               BaseApp.emit event
               BaseApp.logEvent event
             else throw (InsufficientBid "Bid must exceed the price.")

