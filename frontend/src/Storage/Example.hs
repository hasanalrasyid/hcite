{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Storage.Example where

import Data.Functor.Identity (Identity(..))
import GHC.Generics

import Data.Some
import Data.GADT.Show
import Data.GADT.Compare

import Data.Aeson (ToJSON(..), FromJSON(..))

import Data.GADT.Aeson
import Model
import Types

data Foo = Foo { bar :: Bool, baz :: String }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Foo where
instance FromJSON Foo where

data ExampleTag a where
  Tag1 :: ExampleTag (Maybe Env)
  Tag2 :: ExampleTag Foo

instance GEq ExampleTag where
  geq Tag1 Tag1 = Just Refl
  geq Tag2 Tag2 = Just Refl
  geq _ _ = Nothing

instance GCompare ExampleTag where
  gcompare Tag1 Tag1 = GEQ
  gcompare Tag1 _ = GLT
  gcompare _ Tag1 = GGT
  gcompare Tag2 Tag2 = GEQ

instance GShow ExampleTag where
  gshowsPrec _p Tag1 = showString "Tag1"
  gshowsPrec _p Tag2 = showString "Tag2"

  {-
instance ShowTag ExampleTag Identity where
  showTaggedPrec Tag1 = showsPrec
  showTaggedPrec Tag2   = showsPrec
-}

instance GKey ExampleTag where
  toKey (Some Tag1) = "tag1"
  toKey (Some Tag2) = "tag2"

  fromKey t =
    case t of
      "tag1" -> Just (Some Tag1)
      "tag2" -> Just (Some Tag2)
      _ -> Nothing

  keys _ = [Some Tag1, Some Tag2]

instance ToJSONTag ExampleTag Identity where
  toJSONTagged Tag1 (Identity x) = toJSON x
  toJSONTagged Tag2 (Identity x) = toJSON x

instance FromJSONTag ExampleTag Identity where
  parseJSONTagged Tag1 x = Identity <$> parseJSON x
  parseJSONTagged Tag2 x = Identity <$> parseJSON x
