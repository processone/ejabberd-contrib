-- MySQL schema for project Jorge

--
-- Table structure for table `jorge_favorites`
--

DROP TABLE IF EXISTS `jorge_favorites`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `jorge_favorites` (
  `owner_id` int(11) default NULL,
  `peer_name_id` int(11) default NULL,
  `peer_server_id` int(11) default NULL,
  `resource_id` int(11) default NULL,
  `tslice` varchar(20) default NULL,
  `comment` varchar(50) default NULL,
  `ext` int(11) default NULL,
  `link_id` int(10) unsigned NOT NULL auto_increment,
  `vhost` varchar(255) default NULL,
  PRIMARY KEY  (`link_id`),
  KEY `jorge_favorites_ext_idx` (`owner_id`,`ext`),
  KEY `favorites_idx` (`owner_id`,`peer_name_id`,`peer_server_id`,`tslice`,`vhost`)
) ENGINE=InnoDB AUTO_INCREMENT=100 DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `jorge_logger`
--

DROP TABLE IF EXISTS `jorge_logger`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `jorge_logger` (
  `id_user` int(11) default NULL,
  `id_log_detail` int(11) default NULL,
  `id_log_level` int(11) default NULL,
  `log_time` timestamp NOT NULL default CURRENT_TIMESTAMP on update CURRENT_TIMESTAMP,
  `extra` text,
  `vhost` varchar(255) default NULL,
  KEY `logger_idx` (`id_user`,`id_log_detail`,`id_log_level`,`vhost`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `jorge_logger_level_dict`
--

DROP TABLE IF EXISTS `jorge_logger_level_dict`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `jorge_logger_level_dict` (
  `id_level` smallint(6) NOT NULL,
  `level` varchar(20) default NULL,
  `lang` varchar(3) NOT NULL default '',
  PRIMARY KEY  (`id_level`,`lang`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `jorge_logger_dict`
--

DROP TABLE IF EXISTS `jorge_logger_dict`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `jorge_logger_dict` (
  `id_event` smallint(6) NOT NULL,
  `event` text,
  `lang` varchar(3) NOT NULL default 'eng',
  PRIMARY KEY  (`id_event`,`lang`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `jorge_mylinks`
--

DROP TABLE IF EXISTS `jorge_mylinks`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `jorge_mylinks` (
  `id_link` int(11) NOT NULL auto_increment,
  `owner_id` int(11) default NULL,
  `peer_name_id` int(11) default NULL,
  `peer_server_id` int(11) default NULL,
  `datat` text,
  `link` text,
  `description` text,
  `ext` int(11) default NULL,
  `link_id` int(11) default NULL,
  `vhost` varchar(255) default NULL,
  PRIMARY KEY  (`id_link`),
  KEY `mylinks_idx` (`owner_id`,`vhost`)
) ENGINE=InnoDB AUTO_INCREMENT=454 DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `jorge_pref`
--

DROP TABLE IF EXISTS `jorge_pref`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `jorge_pref` (
  `owner_id` int(11) default NULL,
  `pref_id` int(11) default NULL,
  `pref_value` int(11) default NULL,
  `vhost` varchar(255) default NULL,
  KEY `pref_idx` (`owner_id`,`vhost`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `jorge_stats`
--

DROP TABLE IF EXISTS `jorge_stats`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `jorge_stats` (
  `day` date default NULL,
  `hour` tinyint(4) default NULL,
  `value` int(11) default NULL,
  `vhost` varchar(255) default NULL,
  KEY `stats_idx` (`day`,`vhost`),
  PRIMARY KEY  (`day`,`hour`,`vhost`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `pending_del`
--

DROP TABLE IF EXISTS `pending_del`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `pending_del` (
  `owner_id` int(11) default NULL,
  `peer_name_id` int(11) default NULL,
  `date` varchar(20) default NULL,
  `peer_server_id` int(11) default NULL,
  `timeframe` timestamp NOT NULL default CURRENT_TIMESTAMP,
  `type` enum('chat','favorite','mylink','other') default NULL,
  `idx` smallint(6) default NULL,
  `vhost` varchar(255) default NULL,
  KEY `pending_idx` (`owner_id`,`peer_name_id`,`peer_server_id`,`date`,`type`,`idx`,`vhost`),
  KEY `pending_time_idx` (`timeframe`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `jorge_self_names`
--

DROP TABLE IF EXISTS `jorge_self_names`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `jorge_self_names` (
  `owner_id` int(11) NOT NULL,
  `own_name` varchar(60) NOT NULL,
  `vhost` varchar(255) NOT NULL,
  PRIMARY KEY  (`owner_id`,`vhost`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Inserting logger dictionary
--

LOCK TABLES `jorge_logger_dict` WRITE;
/*!40000 ALTER TABLE `jorge_logger_dict` DISABLE KEYS */;
INSERT INTO `jorge_logger_dict` VALUES (1,'Logged in','eng'),(2,'Logged out','eng'),(3,'Login failed','eng'),(4,'Deleted chat thread','eng'),(5,'Deleted whole archive','eng'),(6,'Turned off archivization','eng'),(7,'Turned on archivization','eng'),(8,'Chat exported','eng'),(9,'Deleted entire archive','eng');
/*!40000 ALTER TABLE `jorge_logger_dict` ENABLE KEYS */;
UNLOCK TABLES;

LOCK TABLES `jorge_logger_level_dict` WRITE;
/*!40000 ALTER TABLE `jorge_logger_level_dict` DISABLE KEYS */;
INSERT INTO `jorge_logger_level_dict` VALUES (1,'normal','eng'),(2,'warn','eng'),(3,'alert','eng');
/*!40000 ALTER TABLE `jorge_logger_level_dict` ENABLE KEYS */;
UNLOCK TABLES;

-- EOF
