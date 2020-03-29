#!/usr/bin/env stack
--stack --resolver lts-9.21 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/hcite-filler/stack.yaml

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Taken from http://www.yesodweb.com/book/persistent.
module Filler where

--import           Control.Monad.IO.Class  (liftIO)
--import           Control.Monad.Logger    (runNoLoggingT,runStdoutLoggingT,runStderrLoggingT)
--import           Database.Persist
import           Database.Persist.MySQL
--import           Database.Persist.TH
--import           Control.Monad (void,forM_)
import qualified Text.BibTeX.Parse as B1
import Text.BibTeX.Entry
import Text.Regex
import           Text.Parsec.Prim hiding ((<|>))
import Text.Parsec
import Data.Time
import Data.Maybe
--import Data.List.Split
--import Control.Concurrent
import qualified Data.ByteString.Lazy.Char8 as B
--import Data.Text.Encoding
import qualified Data.Text as Tx
import System.Environment (getArgs)

import Model

  {-
     thesis String Maybe sqltype=enum('Bachelor thesis','Honours thesis','Master thesis','Ph.D. thesis','Diploma thesis','Doctoral thesis','Habilitation thesis')
     -}

connectionInfo :: MySQLConnectInfo
--             = mkMySQLConnectInfo host        user   password   database
connectionInfo = setMySQLConnectInfoCharset 45 $ mkMySQLConnectInfo "localhost" "test" "test"     "example"

readRecords :: B.ByteString -> IO [Reference]
readRecords bibFile = do
  let bib = fromMaybe [] $ parseBib "bibtex" (B.unpack bibFile) :: [T]
--  let (Cons _ _ fBib) = head bib
  putStrLn $ show $ length bib
  mapM genRecord bib
  --putStrLn $ show $ length bibRecs

-- test4 = " @article{Armada_2007, title={A modified finite-lived American exchange option methodology applied to real options valuation}, volume={17}, ISSN={1044-0283}, url={http://dx.doi.org/10.1016/j.gfj.2006.05.006}, DOI={10.1016/j.gfj.2006.05.006}, number={3}, journal={Global Finance Journal}, publisher={Elsevier BV}, author={Armada, Manuel Rocha and Kryzanowski, Lawrence and Pereira, Paulo Jorge}, year={2007}, month={Mar}, pages={419\8211\&438}}\n"

--  insertToDb bibRecs

fillDB :: IO ()
fillDB = do
  putStrLn $ show connectionInfo
  (fBib:_) <- getArgs
  bibFile <- B.readFile fBib
  let bib = fromMaybe [] $ parseBib "bibtex" (B.unpack bibFile) :: [T]
--  let (Cons _ _ fBib) = head bib
  putStrLn $ show $ length bib
  bibRecs <- mapM genRecord bib
  putStrLn $ show $ length bibRecs
-- test4 = " @article{Armada_2007, title={A modified finite-lived American exchange option methodology applied to real options valuation}, volume={17}, ISSN={1044-0283}, url={http://dx.doi.org/10.1016/j.gfj.2006.05.006}, DOI={10.1016/j.gfj.2006.05.006}, number={3}, journal={Global Finance Journal}, publisher={Elsevier BV}, author={Armada, Manuel Rocha and Kryzanowski, Lawrence and Pereira, Paulo Jorge}, year={2007}, month={Mar}, pages={419\8211\&438}}\n"

--  insertToDb bibRecs
  putStrLn $ "------"

  {-
insertToDb r =
  runStdoutLoggingT $ withMySQLPool connectionInfo 10 $ \pool -> liftIO $ do
          flip runSqlPersistMPool pool $ do
            runMigration migrateAll

            insertMany r
            liftIO $ putStrLn "done.."

insertThemAll [] = return ()
insertThemAll (r:rs) = do
  insertMany r
  insertThemAll rs
-}

genRecord :: T -> IO Reference
genRecord (Cons e i f) = do
  curTime <- getCurrentTime
  let today = utctDay curTime
  let totime = timeToTimeOfDay $ utctDayTime curTime
  let blankRef = Reference
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
                (Just $ entryTy e) -- Nothing -- (Just "type             ")
                Nothing -- "thesisId            "
                Nothing -- "expedition          "
                Nothing -- "doi                 "
                Nothing -- "conference          "
                Nothing -- "url                 "
                ( Tx.pack i ) -- "callNumber          "
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
                (Just today) -- "modifiedDate        "
                (Just totime) -- "modifiedTime        "
                (Just "Admin FMIPA Publications (admin@pubs.fmipa.itb.ac.id)")
                1 -- "version             "
  return $ foldl parseFields blankRef $ map (\(a,b) -> (a,Tx.pack b)) f
  where
    entryTy "inproceedings" = "Conference Article"
    entryTy "article" = "Journal Article"
    entryTy "book" = "Book Whole"
    entryTy "incollection" = "Book Chapter"
    entryTy "phdthesis" = "Report"
    entryTy _ = "Miscellaneous"


parseBib :: String -> String -> Maybe [T]
parseBib name bib = f . parseOrError name (B1.skippingLeadingSpace $ B1.skippingSpace B1.file)
                    . sr "^@comment" "comment" $ bib
  where f [x] = Just [x]
        f [] = Nothing -- todo: propagate error, but fail only
        -- if there are no bibtex entries at all (also from other
        -- sources)
        f x = Just x


-- | substitute regex
sr :: String -- ^ regex
   -> String -- ^ replacement
   -> String -- ^ input
   -> String
sr regex replacement input = subRegex (mkRegex regex) input replacement

parseOrError :: SourceName -> Parsec String () c -> String -> c
parseOrError name x y = either (error . unlines . (:[y]) . show) id $ parse x name y

-- reference


parseFields :: Reference -> (String, Tx.Text) -> Reference
parseFields a ("address"      , s) = a {referenceAddress     =      Just s}
parseFields a ("annote"       , s) = a {referenceNotes       =      Just s}
parseFields a ("author"       , s) =
  let allAuthor = map Tx.pack $ B1.splitAuthorList $ Tx.unpack s
   in a { referenceAuthor      =      s
        , referenceFirstAuthor = head allAuthor
        , referenceAuthorCount = length allAuthor
        , referenceCorporateAuthor = Just $ last allAuthor
        }
parseFields a ("booktitle"    , s) = a {referencePublication = Just s}
parseFields a ("chapter"      , _) = a
parseFields a ("crossref"     , _) = a
parseFields a ("edition"      , s) = a {referenceEdition     = Just s}
parseFields a ("editor"       , s) = a {referenceEditor      = Just s}
parseFields a ("howpublished" , _) = a
parseFields a ("institution"  , _) = a
parseFields a ("journal"      , s) = a {referencePublication = Just s}
parseFields a ("key"          , s) = a {referenceKeywords    =   Just s}
parseFields a ("keywords"          , s) = a {referenceKeywords    =   Just s}
parseFields a ("month"        , _) = a
parseFields a ("organization" , _) = a
parseFields a ("school"       , _) = a
parseFields a ("note"         , s) = a {referenceNotes       = Just s}
parseFields a ("number"       , s) = a {referenceCallNumber  =      s}
parseFields a ("pages"        , s) =
      a {referencePages       = Just s
        ,referenceFirstPage   = Just $ readInt s}
parseFields a ("publisher"    , s) = a {referencePublisher   = Just s}
parseFields a ("series"       , s) = a {referenceSeriesTitle = Just s}
parseFields a ("title"        , s) = a {referenceTitle       = s}
parseFields a ("type"         , s) = a {referenceType        = Just s}
parseFields a ("volume"       , s) =
      a { referenceVolume      = Just s
        , referenceVolumeNumeric = Just $ readInt s }
parseFields a ("year"         , s) =
      a {referenceYear        = Just $ readInt s}
parseFields a ("doi"          , s) =
  a {referenceDoi         = Just s
    ,referenceOnlinePublication = "yes"}
parseFields a ("url"          , s) =
  a {referenceUrl         = Just $ Tx.take 255 s
    ,referenceOnlinePublication = "yes"}
parseFields a ("abstract"          , s) = a {referenceAbstract         = Just s}
parseFields a ("issn"          , s) = a {referenceIssn         = Just s}
parseFields a ("isbn"          , s) = a {referenceIsbn = Just s}
parseFields a ("language"          , s) = a {referenceLanguage = Just s}
--biblatex
parseFields a ("date"             ,_) = a
parseFields a ("eprint"           ,_) = a
parseFields a ("eprinttype"       ,_) = a
parseFields a ("eventtitle"       ,_) = a
parseFields a ("file"             ,_) = a
parseFields a ("journaltitle"     ,s) = a { referencePublication = Just s}
parseFields a ("langid"           ,s) = a { referenceLanguage = Just s}
parseFields a ("mrnumber"         ,_) = a
parseFields a ("pagetotal"        ,_) = a
parseFields a ("pmcid"            ,_) = a
parseFields a ("pmid"             ,_) = a
parseFields a ("rights"           ,_) = a
parseFields a ("shortjournal"     ,s) = a { referenceAbbrevJournal = Just s}
parseFields a ("shorttitle"       ,_) = a
parseFields a ("titleaddon"       ,_) = a
parseFields a ("urldate"          ,_) = a
parseFields a ("volumes"          ,_) = a
parseFields a ("zmnumber"         ,_) = a
parseFields a _                    = a


readInt :: Tx.Text -> Int
readInt s = case (reads (Tx.unpack s) :: [(Int,String)]) of
              ((a,_):_) -> a
              [] -> 0

--parseFields a ("title"            ,s) = a { referenceTitle = s}
--parseFields a ("author"           ,s) = a {}
--parseFields a ("booktitle"        ,s) = a {}
--parseFields a ("doi"              ,s) = a {}
--parseFields a ("editor"           ,s) = a {}
--parseFields a ("institution"      ,s) = a {}
--parseFields a ("isbn"             ,s) = a {}
--parseFields a ("issn"             ,s) = a {}
--parseFields a ("issue"            ,s) = a {}
--parseFields a ("keywords"         ,s) = a {}
--parseFields a ("location"         ,s) = a {}
--parseFields a ("note"             ,s) = a {}
--parseFields a ("number"           ,s) = a {}
--parseFields a ("pages"            ,s) = a {}
--parseFields a ("publisher"        ,s) = a {}
--parseFields a ("series"           ,s) = a {}
--parseFields a ("type"             ,s) = a {}
--parseFields a ("url"              ,s) = a {}
--parseFields a ("volume"           ,s) = a {}
