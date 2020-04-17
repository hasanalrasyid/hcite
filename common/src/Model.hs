{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
module Model where

import           Data.Aeson
import           Network.URI

--import           Control.Monad.IO.Class  (liftIO)
--import           Control.Monad.Logger    (runNoLoggingT,runStdoutLoggingT,runStderrLoggingT)
--import           Database.Persist
--import           Database.Persist.MySQL
import           Database.Persist.TH
--import           Control.Monad (void,forM_)
--import qualified Text.BibTeX.Parse as B1
--import Text.BibTeX.Entry
--import Text.Regex
--import           Text.Parsec.Prim hiding ((<|>))
--import Text.Parsec
import Data.Time
import Data.Maybe
--import Data.List.Split
--import Control.Concurrent
--import qualified Data.ByteString.Char8 as B
--import Data.Text.Encoding
import qualified Data.Text as T
--import System.Environment (getArgs)

import GHC.Generics
import Data.Default

-- | Taken from http://www.yesodweb.com/book/persistent.

share [mkPersist sqlSettings, mkMigrate "migrateRefs"] [persistLowerCase|
Pegawai
     nama                T.Text  sqltype=varchar(100)
     nip                 T.Text  sqltype=varchar(50)
     kodeAbsen          Int     sqltype=int(20)
     unit                T.Text  sqltype=varchar(25)
     email               T.Text  sqltype=varchar(50)
     telp                T.Text  sqltype=varchar(100)
     password            T.Text  sqltype=varchar(50)
     tglLahir           Day     sqltype=date
     status              Int     sqltype=smallint(5)
     golongan            Int     sqltype=smallint(5)
     statusAktif        Int     sqltype=smallint(2)
     foto                T.Text  sqltype=varchar(200)
     tglData            UTCTime sqltype=datetime
     fpid                T.Text  sqltype=varchar(10)
     statSpkwt          Int     sqltype=int(1)
  deriving Show Eq Generic

ThesisType
  thesis T.Text
  deriving Show
Reference json
     author                T.Text
     address               T.Text     Maybe
     corporateAuthor       T.Text Maybe    sqltype=varchar(255)
     firstAuthor           T.Text     sqltype=varchar(100)
     authorCount           Int        sqltype=tinyint(3)
     title                 T.Text
     origTitle             T.Text Maybe
     publication            T.Text     Maybe                      sqltype=varchar(255)
     abbrevJournal          T.Text     Maybe                      sqltype=varchar(100)
     year                   Int        Maybe                      sqltype=smallint(6)
     volume                 T.Text     Maybe                      sqltype=varchar(50)
     volumeNumeric          Int        Maybe                      sqltype=smallint(5)
     issue                  T.Text     Maybe                      sqltype=varchar(50)
     pages                  T.Text     Maybe                      sqltype=varchar(50)
     firstPage              Int        Maybe                      sqltype=mediumint(8)
     keywords              T.Text Maybe
     abstract              T.Text Maybe
     edition               T.Text      Maybe                      sqltype=varchar(50)
     editor                T.Text  Maybe
     publisher             T.Text      Maybe                      sqltype=varchar(255)
     place                 T.Text      Maybe                      sqltype=varchar(100)
     medium                T.Text      Maybe                      sqltype=varchar(50)
     seriesEditor          T.Text Maybe
     seriesTitle           T.Text Maybe
     abbrevSeriesTitle     T.Text      Maybe                      sqltype=varchar(100)
     seriesVolume          T.Text      Maybe                      sqltype=varchar(50)
     seriesVolumeNumeric   Int         Maybe                      sqltype=smallint(5)
     seriesIssue           T.Text      Maybe                      sqltype=varchar(50)
     issn                  T.Text      Maybe                      sqltype=varchar(100)
     isbn                  T.Text      Maybe                      sqltype=varchar(100)
     language              T.Text      Maybe                      sqltype=varchar(100)
     summaryLanguage       T.Text      Maybe                      sqltype=varchar(100)
     area                  T.Text      Maybe                      sqltype=varchar(255)
     type                  T.Text      Maybe                      sqltype=varchar(100)
     thesis T.Text Maybe sqltype=enum('Bachelor_thesis','Honours_thesis','Master_thesis','Ph.D._thesis','Diploma_thesis','Doctoral_thesis','Habilitation_thesis')
     expedition            T.Text      Maybe                      sqltype=varchar(255)
     doi                   T.Text      Maybe                      sqltype=varchar(100)
     conference            T.Text      Maybe                      sqltype=varchar(255)
     url                   T.Text                      --           sqltype=varchar(191)
     callNumber            T.Text
     location              T.Text
     contributionId        T.Text     Maybe                       sqltype=varchar(100)
     onlinePublication     T.Text sqltype=enum('no','yes') default='no'
     onlineCitation        T.Text     Maybe                       sqltype=varchar(255)
     file                  T.Text     Maybe                       sqltype=varchar(255)
     notes                 T.Text Maybe
     serial                Int sqltype=mediumint(8)
     origRecord            Int Maybe               sqltype=mediumint(9)
     approved              T.Text sqltype=enum('no','yes') default='no'
     createdDate           Day Maybe
     createdTime           TimeOfDay  Maybe sqltype=time
     createdBy             T.Text   Maybe                         sqltype=varchar(100)
     modifiedDate          Day Maybe
     modifiedTime          TimeOfDay Maybe sqltype=time
     modifiedBy            T.Text   Maybe                         sqltype=varchar(100)
     version               Int sqltype=mediumint(8)  default=1
     Primary serial
     UniqueUrl url
     deriving Eq Show Generic

RelationPR
     pId    PegawaiId
     refIds   [ReferenceId]
  deriving Show
|]

type Personnel = Pegawai

data SimpleRef = SimpleRef { refSerial      :: Int
                           , refAuthor      :: T.Text
                           , refTitle       :: T.Text
                           , refPublication :: T.Text
                           , refYear        :: Int
                           , refVolume      :: T.Text
                           , refPages       :: T.Text
                           , refPublisher   :: T.Text
                           } deriving (Generic,Show)

instance ToJSON SimpleRef
instance FromJSON SimpleRef

data Token = Token { token :: T.Text } deriving (Generic,Show)
instance FromJSON Token
instance ToJSON Token

data Abstract = Abstract { absSerial   :: Int
                         , absAbstract :: T.Text
                         } deriving (Generic,Show)

instance ToJSON Abstract
instance FromJSON Abstract

class FromReference a where
  fromReference :: Reference -> a

instance FromReference Abstract where
  fromReference r = Abstract  (referenceSerial r)
                              (fromMaybe "" $ referenceAbstract r)

instance FromReference SimpleRef where
  fromReference r = SimpleRef ( referenceSerial       r)
                              ( referenceAuthor       r)
                              ( referenceTitle        r)
                              ( fromMaybe ""    $ referencePublication  r)
                              ( fromMaybe 9999  $ referenceYear         r)
                              ( fromMaybe ""    $ referenceVolume       r)
                              ( fromMaybe ""    $ referencePages        r)
                              ( fromMaybe ""    $ referencePublisher    r)

data SearchMode = SAbstract | SAuthor | SKeywords | SOwner deriving (Generic,Ord,Eq)
instance ToJSON SearchMode
instance FromJSON SearchMode

data Search = Search       { searchMode :: SearchMode
                           , searchTerm :: T.Text
                           , searchPage :: Int
                           , searchOwnerID :: Int
                           } deriving (Generic)
instance ToJSON Search
instance FromJSON Search

data OwnerLRef = OwnerLRef { ownPID :: Int
                           , ownLRef :: [Int]
                           } deriving (Generic,Show)

instance ToJSON OwnerLRef
instance FromJSON OwnerLRef

data Person = Person { personName :: T.Text
                     , personId   :: Int
                     } deriving (Generic,Show)
instance ToJSON Person
instance FromJSON Person

data Model = Model
  { records :: Either T.Text [Reference]
  , currentURI :: URI
  , abstractOn :: Int
  , activePanel :: T.Text
  , previousPanel :: T.Text
  } deriving (Eq, Show)

initialModel :: URI -> Model
initialModel uri = Model
    { records = Left "Loading..."
    , currentURI = uri
    , abstractOn = 0
    , activePanel = "landing"
    , previousPanel = "landing"
    }

instance Default Day where
  def = fromGregorian 26 1 2
instance Default UTCTime where
  def = UTCTime def 0
instance Default TimeOfDay where
  def = TimeOfDay 0 0 0

instance Default Pegawai where
  def = Pegawai
                ""      --  nama                T.Text  sqltype=varchar(100)
                ""      --  nip                 T.Text  sqltype=varchar(50)
                0       --  kodeAbsen          Int     sqltype=int(20)
                ""      --  unit                T.Text  sqltype=varchar(25)
                ""      --  email               T.Text  sqltype=varchar(50)
                ""      --  telp                T.Text  sqltype=varchar(100)
                ""      --  password            T.Text  sqltype=varchar(50)
                def     --  tglLahir           Day     sqltype=date
                0       --  status              Int     sqltype=smallint(5)
                0       --  golongan            Int     sqltype=smallint(5)
                0       --  statusAktif        Int     sqltype=smallint(2)
                ""      --  foto                T.Text  sqltype=varchar(200)
                def     --  tglData            UTCTime sqltype=datetime
                ""      --  fpid                T.Text  sqltype=varchar(10)
                0       --  statSpkwt          Int     sqltype=int(1)

instance Default Reference where
  def = Reference
                "author              "
                Nothing -- "address             "
                Nothing -- "corporateAuthor     "
                ("firstAuthor         ")
                0 -- "authorCount         "
                "title               "
                Nothing -- "origTitle           "
                Nothing -- (Just "publication         ")
                Nothing -- (Just "abbrevJournal       ")
                Nothing -- "year                "
                Nothing -- (Just "volume              ")
                Nothing -- (Just 1) -- "volumeNumeric       "
                Nothing -- (Just "issue               ")
                Nothing -- (Just "pages               ")
                Nothing -- (Just 1 ) -- "firstPage           "
                Nothing -- -- "keywords            "
                (Just "abstract            ")
                Nothing -- (Just "edition             ")
                Nothing -- "" -- "editor              "
                Nothing -- (Just "publisher           ")
                Nothing -- (Just "place               ")
                Nothing -- (Just "medium              ")
                Nothing -- ("seriesEditor        ")
                Nothing -- ("seriesTitle         ")
                Nothing -- (Just "abbrevSeriesTitle   ")
                Nothing -- (Just "seriesVolume        ")
                Nothing -- (Just 1) -- "seriesVolumeNumeric "
                Nothing -- (Just "seriesIssue         ")
                Nothing -- (Just "issn                ")
                Nothing -- (Just "isbn                ")
                Nothing -- (Just "language            ")
                Nothing -- (Just "summaryLanguage     ")
                Nothing -- (Just "area                ")
                Nothing -- (Just "type             ")
                Nothing -- "thesisId            "
                Nothing -- "expedition          "
                Nothing -- "doi                 "
                Nothing -- "conference          "
                "" -- "url                 "
                ""  -- "callNumber          "
                "" -- "location            "
                Nothing -- "contributionId      "
                "no" -- "onlinePublication   "
                Nothing -- "onlineCitation      "
                Nothing -- "file                "
                Nothing -- "notes               "
                0 -- 1 -- "serial              "
                Nothing -- (Just 1) -- "origRecord          "
                "no" -- "approved            "
                Nothing -- "createdDate         "
                Nothing -- "createdTime         "
                (Just "Dede Enan (dede@fi.itb.ac.id)")
                (Just def) -- "modifiedDate        "
                (Just def) -- "modifiedTime        "
                (Just "Admin FMIPA Publications (admin@pubs.fmipa.itb.ac.id)")
                1 -- "version             "
