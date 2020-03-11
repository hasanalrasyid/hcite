module Action where

import           Network.URI

import           Model

data Action
    = FetchRecords
    | SetRecords (Either String [Reference])
    | SaveRecord Reference
    | SetRecord (Either String Reference)
    | HandleURI URI
    | ChangeURI URI
    | ToggleAbstract Int
    | ActivatePanel String Int
    | NoOp

initAction :: Action
initAction = FetchRecords
