-- MySQL dump 10.14  Distrib 5.5.42-MariaDB, for Linux (x86_64)
--
-- Host: localhost    Database: pubs
-- ------------------------------------------------------
-- Server version	5.5.42-MariaDB-log

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `refs`
--

DROP TABLE IF EXISTS `refs`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `refs` (
  `author` text,
  `address` text,
  `corporate_author` varchar(255) DEFAULT NULL,
  `first_author` varchar(100) DEFAULT NULL,
  `author_count` tinyint(3) unsigned DEFAULT NULL,
  `title` text,
  `orig_title` text,
  `publication` varchar(255) DEFAULT NULL,
  `abbrev_journal` varchar(100) DEFAULT NULL,
  `year` smallint(6) DEFAULT NULL,
  `volume` varchar(50) DEFAULT NULL,
  `volume_numeric` smallint(5) unsigned DEFAULT NULL,
  `issue` varchar(50) DEFAULT NULL,
  `pages` varchar(50) DEFAULT NULL,
  `first_page` mediumint(8) unsigned DEFAULT NULL,
  `keywords` text,
  `abstract` text,
  `edition` varchar(50) DEFAULT NULL,
  `editor` text,
  `publisher` varchar(255) DEFAULT NULL,
  `place` varchar(100) DEFAULT NULL,
  `MEDIUM` varchar(50) DEFAULT NULL,
  `series_editor` text,
  `series_title` text,
  `abbrev_series_title` varchar(100) DEFAULT NULL,
  `series_volume` varchar(50) DEFAULT NULL,
  `series_volume_numeric` smallint(5) unsigned DEFAULT NULL,
  `series_issue` varchar(50) DEFAULT NULL,
  `issn` varchar(100) DEFAULT NULL,
  `isbn` varchar(100) DEFAULT NULL,
  `language` varchar(100) DEFAULT NULL,
  `summary_language` varchar(100) DEFAULT NULL,
  `area` varchar(255) DEFAULT NULL,
  `TYPE` varchar(100) DEFAULT NULL,
  `thesis` enum('Bachelor''s thesis','Honours thesis','Master''s thesis','Ph.D. thesis','Diploma thesis','Doctoral thesis','Habilitation thesis') DEFAULT NULL,
  `expedition` varchar(255) DEFAULT NULL,
  `doi` varchar(100) DEFAULT NULL,
  `conference` varchar(255) DEFAULT NULL,
  `url` varchar(255) DEFAULT NULL,
  `call_number` text,
  `location` text,
  `contribution_id` varchar(100) DEFAULT NULL,
  `online_publication` enum('no','yes') NOT NULL DEFAULT 'no',
  `online_citation` varchar(255) DEFAULT NULL,
  `FILE` varchar(255) DEFAULT NULL,
  `notes` text,
  `serial` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `orig_record` mediumint(9) DEFAULT NULL,
  `approved` enum('no','yes') NOT NULL DEFAULT 'no',
  `created_date` date DEFAULT NULL,
  `created_time` time DEFAULT NULL,
  `created_by` varchar(100) DEFAULT NULL,
  `modified_date` date DEFAULT NULL,
  `modified_time` time DEFAULT NULL,
  `modified_by` varchar(100) DEFAULT NULL,
  `version` mediumint(8) unsigned DEFAULT '1',
  PRIMARY KEY (`serial`)
) ENGINE=MyISAM AUTO_INCREMENT=1351 DEFAULT CHARSET=utf8;
