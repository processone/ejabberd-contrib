CREATE DATABASE IF NOT EXISTS ejabberd CHARACTER SET utf8 COLLATE utf8_general_ci;

USE ejabberd;

SET table_type=InnoDB;

CREATE TABLE archive_collections(id INTEGER NOT NULL AUTO_INCREMENT,
                                 prev_id INTEGER,
                                 next_id INTEGER,
                                 us VARCHAR(2047) NOT NULL,
                                 with_user VARCHAR(1023) NOT NULL,
                                 with_server VARCHAR(1023) NOT NULL,
                                 with_resource VARCHAR(1023) NOT NULL,
                                 utc DATETIME NOT NULL,
                                 change_by VARCHAR(3071),
                                 change_utc DATETIME,
                                 deleted TINYINT,
                                 subject VARCHAR(1023),
                                 thread VARCHAR(1023),
                                 crypt TINYINT,
                                 extra VARCHAR(32767),
                                 PRIMARY KEY(id))
                                 CHARACTER SET utf8
                                 COLLATE utf8_general_ci;
CREATE INDEX IDX_archive_colls_with ON archive_collections(us(16),with_user(8),with_server(8),utc);
CREATE INDEX IDX_archive_colls_prev_id ON archive_collections(prev_id);
CREATE INDEX IDX_archive_colls_next_id ON archive_collections(next_id);
CREATE INDEX IDX_archive_colls_utc ON archive_collections(us(16),utc);
CREATE INDEX IDX_archive_colls_change ON archive_collections(deleted,change_utc);

CREATE TABLE archive_messages(id INTEGER NOT NULL AUTO_INCREMENT,
                              coll_id INTEGER NOT NULL,
                              utc DATETIME NOT NULL,
                              dir TINYINT,
                              body VARCHAR(63488),
                              name VARCHAR(1023),
                              PRIMARY KEY(id))
                              CHARACTER SET utf8
                              COLLATE utf8_general_ci;
CREATE INDEX IDX_archive_msgs_coll_id ON archive_messages(coll_id,utc);

CREATE TABLE archive_jid_prefs(us VARCHAR(2047) NOT NULL,
                               with_user VARCHAR(1023) NOT NULL,
                               with_server VARCHAR(1023) NOT NULL,
                               with_resource VARCHAR(1023) NOT NULL,
                               save TINYINT,
                               expire INTEGER,
                               otr TINYINT,
                               PRIMARY KEY  (us(16),with_user(8),with_server(8),with_resource(8)))
                               CHARACTER SET utf8
                               COLLATE utf8_general_ci;

CREATE TABLE archive_global_prefs(us VARCHAR(2047) NOT NULL,
                                  save TINYINT,
                                  expire INTEGER,
                                  otr TINYINT,
                                  method_auto TINYINT,
                                  method_local TINYINT,
                                  method_manual TINYINT,
                                  auto_save TINYINT,
                                  PRIMARY KEY  (us(16)))
                                  CHARACTER SET utf8
                                  COLLATE utf8_general_ci;

DELIMITER |

CREATE TRIGGER archive_collections_delete BEFORE DELETE ON archive_collections
FOR EACH ROW
BEGIN
  DELETE FROM archive_messages WHERE coll_id = OLD.id;
END;
|

CREATE TRIGGER archive_collections_update BEFORE UPDATE ON archive_collections
FOR EACH ROW
BEGIN
  IF NEW.deleted = 1 THEN
    DELETE FROM archive_messages WHERE coll_id = NEW.id;
  END IF;
END;
|

DELIMITER ;
