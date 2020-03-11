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
module Model where

--import           Data.Aeson
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
--import Data.Maybe
--import Data.List.Split
--import Control.Concurrent
--import qualified Data.ByteString.Char8 as B
--import Data.Text.Encoding
--import qualified Data.Text as Tx
--import System.Environment (getArgs)

-- | Taken from http://www.yesodweb.com/book/persistent.

share [mkPersist sqlSettings, mkMigrate "migrateRefs"] [persistLowerCase|
ThesisType
  thesis String
  deriving Show
Reference json
     author                String
     address               String     Maybe
     corporateAuthor       String Maybe    sqltype=varchar(255)
     firstAuthor           String     sqltype=varchar(100)
     authorCount           Int        sqltype=tinyint(3)
     title                 String
     origTitle             String Maybe
     publication            String     Maybe                      sqltype=varchar(255)
     abbrevJournal          String     Maybe                      sqltype=varchar(100)
     year                   Int        Maybe                      sqltype=smallint(6)
     volume                 String     Maybe                      sqltype=varchar(50)
     volumeNumeric          Int        Maybe                      sqltype=smallint(5)
     issue                  String     Maybe                      sqltype=varchar(50)
     pages                  String     Maybe                      sqltype=varchar(50)
     firstPage              Int        Maybe                      sqltype=mediumint(8)
     keywords              String Maybe
     abstract              String Maybe
     edition               String      Maybe                      sqltype=varchar(50)
     editor                String  Maybe
     publisher             String      Maybe                      sqltype=varchar(255)
     place                 String      Maybe                      sqltype=varchar(100)
     medium                String      Maybe                      sqltype=varchar(50)
     seriesEditor          String Maybe
     seriesTitle           String Maybe
     abbrevSeriesTitle     String      Maybe                      sqltype=varchar(100)
     seriesVolume          String      Maybe                      sqltype=varchar(50)
     seriesVolumeNumeric   Int         Maybe                      sqltype=smallint(5)
     seriesIssue           String      Maybe                      sqltype=varchar(50)
     issn                  String      Maybe                      sqltype=varchar(100)
     isbn                  String      Maybe                      sqltype=varchar(100)
     language              String      Maybe                      sqltype=varchar(100)
     summaryLanguage       String      Maybe                      sqltype=varchar(100)
     area                  String      Maybe                      sqltype=varchar(255)
     type                  String      Maybe                      sqltype=varchar(100)
     thesis String Maybe sqltype=enum('Bachelor_thesis','Honours_thesis','Master_thesis','Ph.D._thesis','Diploma_thesis','Doctoral_thesis','Habilitation_thesis')
     expedition            String      Maybe                      sqltype=varchar(255)
     doi                   String      Maybe                      sqltype=varchar(100)
     conference            String      Maybe                      sqltype=varchar(255)
     url                   String      Maybe                      sqltype=varchar(255)
     callNumber            String
     location              String
     contributionId        String     Maybe                       sqltype=varchar(100)
     onlinePublication     String sqltype=enum('no','yes') default='no'
     onlineCitation        String     Maybe                       sqltype=varchar(255)
     file                  String     Maybe                       sqltype=varchar(255)
     notes                 String Maybe
     serial                Int sqltype=mediumint(8)
     origRecord            Int Maybe               sqltype=mediumint(9)
     approved              String sqltype=enum('no','yes') default='no'
     createdDate           Day Maybe
     createdTime           TimeOfDay  Maybe sqltype=time
     createdBy             String   Maybe                         sqltype=varchar(100)
     modifiedDate          Day Maybe
     modifiedTime          TimeOfDay Maybe sqltype=time
     modifiedBy            String   Maybe                         sqltype=varchar(100)
     version               Int sqltype=mediumint(8)  default=1
     Primary serial
     deriving Eq Show
|]

data Model = Model
  { records :: Either String [Reference]
  , currentURI :: URI
  , abstractOn :: Int
  , activePanel :: String
  , previousPanel :: String
  } deriving (Eq, Show)

initialModel :: URI -> Model
initialModel uri = Model
    { records = Left "Loading..."
    , currentURI = uri
    , abstractOn = 0
    , activePanel = "landing"
    , previousPanel = "landing"
    }

