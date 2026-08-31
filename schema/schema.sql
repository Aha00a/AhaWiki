-- MySQL dump 10.13  Distrib #.#.#, for OS
--
-- Host: (from .env)    Database: (from .env)
-- ------------------------------------------------------
-- Server version	11.4.12-MariaDB-log

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!50503 SET NAMES utf8mb4 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `AccessLog`
--

DROP TABLE IF EXISTS `AccessLog`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `AccessLog` (
  `seq` int(11) NOT NULL AUTO_INCREMENT,
  `site` int(11) NOT NULL,
  `user` int(11) DEFAULT NULL,
  `ipDeny` int(11) DEFAULT NULL,
  `dateInserted` datetime NOT NULL DEFAULT current_timestamp(),
  `method` varchar(16) NOT NULL,
  `status` int(11) NOT NULL,
  `durationMilli` int(11) NOT NULL,
  `scheme` varchar(10) NOT NULL,
  `host` varchar(255) NOT NULL,
  `uri` varchar(4096) NOT NULL,
  `remoteAddress` varchar(46) NOT NULL,
  `userAgent` varchar(512) NOT NULL,
  PRIMARY KEY (`seq`),
  KEY `AccessLog_Site_seq_fk` (`site`),
  KEY `AccessLog_remoteAddress_index` (`remoteAddress`),
  KEY `AccessLog_IpDeny_seq_fk` (`ipDeny`),
  KEY `AccessLog_User_seq_fk` (`user`),
  KEY `AccessLog_dateInserted_index` (`dateInserted`),
  CONSTRAINT `AccessLog_IpDeny_seq_fk` FOREIGN KEY (`ipDeny`) REFERENCES `IpDeny` (`seq`) ON DELETE SET NULL,
  CONSTRAINT `AccessLog_Site_seq_fk` FOREIGN KEY (`site`) REFERENCES `Site` (`seq`),
  CONSTRAINT `AccessLog_User_seq_fk` FOREIGN KEY (`user`) REFERENCES `User` (`seq`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `Attachment`
--

DROP TABLE IF EXISTS `Attachment`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Attachment` (
  `seq` bigint(20) NOT NULL AUTO_INCREMENT,
  `dateInserted` datetime NOT NULL DEFAULT current_timestamp(),
  `dateUpdated` datetime NOT NULL DEFAULT current_timestamp(),
  `dateUploaded` datetime DEFAULT NULL,
  `dateDeleted` datetime DEFAULT NULL,
  `site` int(11) NOT NULL,
  `pageName` varchar(255) NOT NULL,
  `user` int(11) DEFAULT NULL,
  `uploaderEmail` varchar(255) DEFAULT NULL,
  `originalFilename` varchar(255) NOT NULL,
  `storedFilename` varchar(255) NOT NULL,
  `bucket` varchar(255) NOT NULL,
  `objectKey` varchar(512) NOT NULL,
  `contentType` varchar(255) NOT NULL,
  `fileSize` bigint(20) NOT NULL,
  `sha256` char(64) DEFAULT NULL,
  `etag` varchar(255) DEFAULT NULL,
  `status` enum('Initiated','Uploaded','Verified','Deleted','Failed') NOT NULL,
  `imageWidth` int(11) DEFAULT NULL,
  `imageHeight` int(11) DEFAULT NULL,
  PRIMARY KEY (`seq`),
  UNIQUE KEY `Attachment_objectKey_uindex` (`objectKey`),
  KEY `Attachment_site_pageName_dateInserted_index` (`site`,`pageName`,`dateInserted`),
  KEY `Attachment_site_status_dateInserted_index` (`site`,`status`,`dateInserted`),
  KEY `Attachment_user_dateInserted_index` (`user`,`dateInserted`),
  KEY `Attachment_dateDeleted_index` (`dateDeleted`),
  CONSTRAINT `Attachment_Site_seq_fk` FOREIGN KEY (`site`) REFERENCES `Site` (`seq`),
  CONSTRAINT `Attachment_User_seq_fk` FOREIGN KEY (`user`) REFERENCES `User` (`seq`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `CacheCrawler`
--

DROP TABLE IF EXISTS `CacheCrawler`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `CacheCrawler` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `url` varchar(512) NOT NULL,
  `dateInserted` datetime NOT NULL DEFAULT current_timestamp(),
  `dateUpdated` datetime NOT NULL DEFAULT current_timestamp() ON UPDATE current_timestamp(),
  `title` varchar(256) NOT NULL,
  `image` varchar(512) NOT NULL,
  `description` varchar(512) NOT NULL,
  `status` enum('Queued','Done') NOT NULL DEFAULT 'Queued',
  PRIMARY KEY (`id`),
  UNIQUE KEY `CacheCrawler_url_unique` (`url`),
  KEY `CacheCrawler_dateUpdated_index` (`dateUpdated`),
  KEY `CacheCrawler_status_index` (`status`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `CalculatedCosineSimilarity`
--

DROP TABLE IF EXISTS `CalculatedCosineSimilarity`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `CalculatedCosineSimilarity` (
  `site1` int(11) NOT NULL,
  `name1` varchar(255) NOT NULL DEFAULT '',
  `site2` int(11) NOT NULL,
  `name2` varchar(255) NOT NULL DEFAULT '',
  `similarity` double DEFAULT NULL,
  PRIMARY KEY (`site1`,`name1`,`site2`,`name2`),
  KEY `CalculatedCosineSimilarity_site1_name1_similarity_index` (`site1`,`name1`,`similarity`),
  KEY `CalculatedCosineSimilarity_site2_name2_index` (`site2`,`name2`),
  CONSTRAINT `CalculatedCosineSimilarity_name1_PageMeta_site_name_fk` FOREIGN KEY (`site1`, `name1`) REFERENCES `PageMeta` (`site`, `name`),
  CONSTRAINT `CalculatedCosineSimilarity_name2_PageMeta_site_name_fk` FOREIGN KEY (`site2`, `name2`) REFERENCES `PageMeta` (`site`, `name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `CalculatedLink`
--

DROP TABLE IF EXISTS `CalculatedLink`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `CalculatedLink` (
  `site` int(11) NOT NULL,
  `src` varchar(255) NOT NULL,
  `dst` varchar(255) NOT NULL,
  `alias` varchar(255) NOT NULL,
  PRIMARY KEY (`site`,`src`,`dst`,`alias`),
  KEY `CalculatedLink_site_dst_src_alias_index` (`site`,`dst`,`src`,`alias`),
  CONSTRAINT `fkLinkSite` FOREIGN KEY (`site`, `src`) REFERENCES `Page` (`site`, `name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `CalculatedSchemaOrg`
--

DROP TABLE IF EXISTS `CalculatedSchemaOrg`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `CalculatedSchemaOrg` (
  `site` int(11) NOT NULL,
  `page` varchar(255) NOT NULL,
  `cls` varchar(50) NOT NULL,
  `prop` varchar(50) NOT NULL,
  `value` varchar(255) NOT NULL,
  PRIMARY KEY (`site`,`page`,`cls`,`prop`,`value`),
  KEY `CalculatedSchemaOrg_site_value_page_index` (`site`,`value`,`page`),
  CONSTRAINT `fkTermFrequencyPage` FOREIGN KEY (`site`, `page`) REFERENCES `Page` (`site`, `name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `CalculatedTerm`
--

DROP TABLE IF EXISTS `CalculatedTerm`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `CalculatedTerm` (
  `seq` bigint(20) NOT NULL AUTO_INCREMENT,
  `dateInserted` datetime NOT NULL DEFAULT current_timestamp(),
  `term` varchar(255) NOT NULL,
  PRIMARY KEY (`seq`),
  UNIQUE KEY `term` (`term`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `CalculatedTermFrequency`
--

DROP TABLE IF EXISTS `CalculatedTermFrequency`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `CalculatedTermFrequency` (
  `site` int(11) NOT NULL,
  `name` varchar(255) NOT NULL,
  `term` bigint(20) NOT NULL,
  `frequency` int(11) NOT NULL,
  PRIMARY KEY (`site`,`name`,`term`),
  KEY `CalculatedTermFrequency_CalculatedTerm_seq_fk` (`term`),
  KEY `CalculatedTermFrequency_term_site_name_frequency_index` (`term`,`site`,`name`,`frequency`),
  CONSTRAINT `CalculatedTermFrequency_CalculatedTerm_seq_fk` FOREIGN KEY (`term`) REFERENCES `CalculatedTerm` (`seq`),
  CONSTRAINT `CalculatedTermFrequency_Page_site_name_fk` FOREIGN KEY (`site`, `name`) REFERENCES `Page` (`site`, `name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `CalculatedTermFrequencyNorm`
--

DROP TABLE IF EXISTS `CalculatedTermFrequencyNorm`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `CalculatedTermFrequencyNorm` (
  `site` int(11) NOT NULL,
  `name` varchar(255) NOT NULL,
  `norm` double NOT NULL,
  PRIMARY KEY (`site`,`name`),
  CONSTRAINT `CalculatedTermFrequencyNorm_PageMeta_site_name_fk` FOREIGN KEY (`site`, `name`) REFERENCES `PageMeta` (`site`, `name`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `Config`
--

DROP TABLE IF EXISTS `Config`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Config` (
  `site` int(11) NOT NULL,
  `k` varchar(512) NOT NULL,
  `v` varchar(512) NOT NULL,
  `created` datetime NOT NULL DEFAULT current_timestamp(),
  `updated` datetime NOT NULL DEFAULT current_timestamp(),
  PRIMARY KEY (`site`,`k`),
  CONSTRAINT `fkConfigSiteSiteSeq` FOREIGN KEY (`site`) REFERENCES `Site` (`seq`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `GeocodeCache`
--

DROP TABLE IF EXISTS `GeocodeCache`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `GeocodeCache` (
  `address` varchar(255) NOT NULL,
  `lat` double NOT NULL,
  `lng` double NOT NULL,
  `created` datetime NOT NULL DEFAULT current_timestamp(),
  PRIMARY KEY (`address`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `IpDeny`
--

DROP TABLE IF EXISTS `IpDeny`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `IpDeny` (
  `seq` int(11) NOT NULL AUTO_INCREMENT,
  `accessLog` int(11) DEFAULT NULL,
  `dateInserted` datetime NOT NULL DEFAULT current_timestamp(),
  `ip` varchar(46) NOT NULL,
  `reason` varchar(255) NOT NULL,
  PRIMARY KEY (`seq`),
  KEY `IpDeny_dateInserted_index` (`dateInserted`),
  KEY `IpDeny_ip_index` (`ip`),
  KEY `IpDeny_AccessLog_seq_fk` (`accessLog`),
  CONSTRAINT `IpDeny_AccessLog_seq_fk` FOREIGN KEY (`accessLog`) REFERENCES `AccessLog` (`seq`) ON DELETE SET NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `Page`
--

DROP TABLE IF EXISTS `Page`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Page` (
  `site` int(11) NOT NULL,
  `name` varchar(255) NOT NULL DEFAULT '',
  `revision` int(11) NOT NULL DEFAULT 0,
  `dateTime` datetime(6) NOT NULL,
  `user` int(11) NOT NULL,
  `remoteAddress` text DEFAULT NULL,
  `comment` text NOT NULL,
  `isMinorEdit` tinyint(1) NOT NULL DEFAULT 0,
  `viaApi` tinyint(1) NOT NULL DEFAULT 0,
  `userApiKey` bigint(20) DEFAULT NULL,
  `content` longtext DEFAULT NULL,
  PRIMARY KEY (`site`,`name`,`revision`),
  KEY `Page_User_seq_fk` (`user`),
  KEY `Page_site_isMinorEdit_name_revision_index` (`site`,`isMinorEdit`,`name`,`revision`),
  KEY `Page_site_name_revision_dateTime_user_index` (`site`,`name`,`revision`,`dateTime`,`user`),
  KEY `Page_UserApiKey_seq_fk` (`userApiKey`),
  CONSTRAINT `Page_Site_seq_fk` FOREIGN KEY (`site`) REFERENCES `Site` (`seq`),
  CONSTRAINT `Page_UserApiKey_seq_fk` FOREIGN KEY (`userApiKey`) REFERENCES `UserApiKey` (`seq`) ON DELETE SET NULL,
  CONSTRAINT `Page_User_seq_fk` FOREIGN KEY (`user`) REFERENCES `User` (`seq`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `PageMeta`
--

DROP TABLE IF EXISTS `PageMeta`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `PageMeta` (
  `site` int(11) NOT NULL,
  `name` varchar(255) NOT NULL,
  `dateInserted` datetime NOT NULL DEFAULT current_timestamp(),
  `dateUpdated` datetime DEFAULT NULL,
  `revision` int(11) NOT NULL,
  `image` varchar(512) DEFAULT NULL,
  `description` varchar(512) DEFAULT NULL,
  `size` bigint(20) NOT NULL DEFAULT 0,
  PRIMARY KEY (`site`,`name`),
  KEY `PageMeta_Page_site_name_revision_fk` (`site`,`name`,`revision`),
  KEY `PageMeta_site_dateUpdated_name_index` (`site`,`dateUpdated`,`name`),
  CONSTRAINT `PageMeta_Page_site_name_revision_fk` FOREIGN KEY (`site`, `name`, `revision`) REFERENCES `Page` (`site`, `name`, `revision`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `Permission`
--

DROP TABLE IF EXISTS `Permission`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Permission` (
  `site` int(11) NOT NULL,
  `target` varchar(255) NOT NULL,
  `targetType` enum('All','Exact','StartsWith','EndsWith','RegularExpression') NOT NULL DEFAULT 'Exact',
  `actor` varchar(255) NOT NULL,
  `actorType` enum('All','Login','Exact','Domain') NOT NULL DEFAULT 'Exact',
  `action` int(11) NOT NULL,
  `dateInserted` datetime NOT NULL DEFAULT current_timestamp(),
  `dateUpdated` datetime NOT NULL DEFAULT current_timestamp() ON UPDATE current_timestamp(),
  PRIMARY KEY (`site`,`targetType`,`target`,`actorType`,`actor`),
  KEY `Permission_site_index` (`site`),
  CONSTRAINT `Permission_Site_seq_fk` FOREIGN KEY (`site`) REFERENCES `Site` (`seq`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `Site`
--

DROP TABLE IF EXISTS `Site`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Site` (
  `seq` int(11) NOT NULL AUTO_INCREMENT,
  `created` datetime NOT NULL DEFAULT current_timestamp(),
  `updated` datetime NOT NULL DEFAULT current_timestamp(),
  `name` varchar(200) NOT NULL,
  `abbr` varchar(200) NOT NULL,
  `mainDomain` varchar(255) NOT NULL DEFAULT '',
  `publicListedOrder` decimal(10,2) DEFAULT NULL,
  PRIMARY KEY (`seq`),
  UNIQUE KEY `Site_abbr_uindex` (`abbr`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `SiteAdmin`
--

DROP TABLE IF EXISTS `SiteAdmin`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `SiteAdmin` (
  `site` int(11) NOT NULL,
  `user` int(11) NOT NULL,
  `dateInserted` datetime NOT NULL DEFAULT current_timestamp(),
  PRIMARY KEY (`site`,`user`),
  KEY `SiteAdmin_User_seq_fk` (`user`),
  CONSTRAINT `SiteAdmin_Site_seq_fk` FOREIGN KEY (`site`) REFERENCES `Site` (`seq`),
  CONSTRAINT `SiteAdmin_User_seq_fk` FOREIGN KEY (`user`) REFERENCES `User` (`seq`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `SiteDomain`
--

DROP TABLE IF EXISTS `SiteDomain`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `SiteDomain` (
  `created` datetime NOT NULL DEFAULT current_timestamp(),
  `site` int(11) NOT NULL,
  `domain` varchar(255) NOT NULL,
  PRIMARY KEY (`site`,`domain`),
  UNIQUE KEY `SiteDomain_domain_uindex` (`domain`),
  CONSTRAINT `SiteDomain_Site_seq_fk` FOREIGN KEY (`site`) REFERENCES `Site` (`seq`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `User`
--

DROP TABLE IF EXISTS `User`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `User` (
  `seq` int(11) NOT NULL AUTO_INCREMENT,
  `created` datetime NOT NULL DEFAULT current_timestamp(),
  `updated` datetime NOT NULL DEFAULT current_timestamp() ON UPDATE current_timestamp(),
  `nickname` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
  `profileImageUrl` varchar(2048) DEFAULT NULL,
  `dateNicknameChanged` datetime DEFAULT NULL,
  PRIMARY KEY (`seq`),
  UNIQUE KEY `User_nickname_lower_uindex` (`nickname`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `UserApiKey`
--

DROP TABLE IF EXISTS `UserApiKey`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `UserApiKey` (
  `seq` bigint(20) NOT NULL AUTO_INCREMENT,
  `user` int(11) NOT NULL,
  `keyHash` varchar(64) NOT NULL,
  `keyPrefix` varchar(32) NOT NULL,
  `name` varchar(255) NOT NULL,
  `dateInserted` datetime NOT NULL DEFAULT current_timestamp(),
  `dateLastUsed` datetime DEFAULT NULL,
  `dateRevoked` datetime DEFAULT NULL,
  PRIMARY KEY (`seq`),
  UNIQUE KEY `UserApiKey_keyHash_uindex` (`keyHash`),
  KEY `UserApiKey_user_dateRevoked_index` (`user`,`dateRevoked`),
  CONSTRAINT `UserApiKey_User_seq_fk` FOREIGN KEY (`user`) REFERENCES `User` (`seq`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `UserEmail`
--

DROP TABLE IF EXISTS `UserEmail`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `UserEmail` (
  `user` int(11) NOT NULL,
  `email` varchar(255) NOT NULL,
  `isPrimary` tinyint(1) NOT NULL DEFAULT 0,
  `created` datetime NOT NULL DEFAULT current_timestamp(),
  PRIMARY KEY (`user`,`email`),
  UNIQUE KEY `UserEmail_email_uindex` (`email`),
  CONSTRAINT `UserEmail_User_seq_fk` FOREIGN KEY (`user`) REFERENCES `User` (`seq`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `UserNicknameChangeRequest`
--

DROP TABLE IF EXISTS `UserNicknameChangeRequest`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `UserNicknameChangeRequest` (
  `seq` bigint(20) NOT NULL AUTO_INCREMENT,
  `user` int(11) NOT NULL,
  `requestedNickname` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
  `status` varchar(20) NOT NULL,
  `requestedBy` int(11) NOT NULL,
  `reviewedBy` int(11) DEFAULT NULL,
  `rejectReason` varchar(1000) DEFAULT NULL,
  `dateInserted` datetime NOT NULL DEFAULT current_timestamp(),
  `dateReviewed` datetime DEFAULT NULL,
  PRIMARY KEY (`seq`),
  KEY `UserNicknameChangeRequest_RequestedBy_User_seq_fk` (`requestedBy`),
  KEY `UserNicknameChangeRequest_ReviewedBy_User_seq_fk` (`reviewedBy`),
  KEY `UserNicknameChangeRequest_status_dateInserted_index` (`status`,`dateInserted`),
  KEY `UserNicknameChangeRequest_user_status_index` (`user`,`status`),
  KEY `UserNicknameChangeRequest_requestedNickname_status_index` (`requestedNickname`,`status`),
  CONSTRAINT `UserNicknameChangeRequest_RequestedBy_User_seq_fk` FOREIGN KEY (`requestedBy`) REFERENCES `User` (`seq`),
  CONSTRAINT `UserNicknameChangeRequest_ReviewedBy_User_seq_fk` FOREIGN KEY (`reviewedBy`) REFERENCES `User` (`seq`),
  CONSTRAINT `UserNicknameChangeRequest_User_seq_fk` FOREIGN KEY (`user`) REFERENCES `User` (`seq`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `UserNicknameHistory`
--

DROP TABLE IF EXISTS `UserNicknameHistory`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `UserNicknameHistory` (
  `seq` bigint(20) NOT NULL AUTO_INCREMENT,
  `user` int(11) NOT NULL,
  `old` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
  `new` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
  `changedBy` int(11) NOT NULL,
  `dateInserted` datetime NOT NULL DEFAULT current_timestamp(),
  PRIMARY KEY (`seq`),
  KEY `UserNicknameHistory_ChangedBy_User_seq_fk` (`changedBy`),
  KEY `UserNicknameHistory_user_dateInserted_index` (`user`,`dateInserted`),
  CONSTRAINT `UserNicknameHistory_ChangedBy_User_seq_fk` FOREIGN KEY (`changedBy`) REFERENCES `User` (`seq`),
  CONSTRAINT `UserNicknameHistory_User_seq_fk` FOREIGN KEY (`user`) REFERENCES `User` (`seq`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `UserViewHistory`
--

DROP TABLE IF EXISTS `UserViewHistory`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `UserViewHistory` (
  `seq` bigint(20) NOT NULL AUTO_INCREMENT,
  `user` int(11) NOT NULL,
  `site` int(11) NOT NULL,
  `pageName` varchar(255) NOT NULL,
  `dateInserted` datetime NOT NULL DEFAULT current_timestamp(),
  PRIMARY KEY (`seq`),
  KEY `UserViewHistory_user_dateInserted_index` (`user`,`dateInserted`),
  KEY `UserViewHistory_site_pageName_dateInserted_index` (`site`,`pageName`,`dateInserted`),
  KEY `UserViewHistory_dateInserted_index` (`dateInserted`),
  CONSTRAINT `UserViewHistory_Site_seq_fk` FOREIGN KEY (`site`) REFERENCES `Site` (`seq`) ON DELETE CASCADE,
  CONSTRAINT `UserViewHistory_User_seq_fk` FOREIGN KEY (`user`) REFERENCES `User` (`seq`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `play_evolutions`
--

DROP TABLE IF EXISTS `play_evolutions`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `play_evolutions` (
  `id` int(11) NOT NULL,
  `hash` varchar(255) NOT NULL,
  `applied_at` timestamp NOT NULL DEFAULT current_timestamp(),
  `apply_script` mediumtext DEFAULT NULL,
  `revert_script` mediumtext DEFAULT NULL,
  `state` varchar(255) DEFAULT NULL,
  `last_problem` mediumtext DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
/*!40101 SET character_set_client = @saved_cs_client */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;


