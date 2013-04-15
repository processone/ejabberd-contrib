CREATE TABLE archive_collections(id INTEGER NOT NULL,
                                 prev_id INTEGER,
                                 next_id INTEGER,
                                 us VARCHAR(2047) NOT NULL,
                                 with_user VARCHAR(1023) NOT NULL,
                                 with_server VARCHAR(1023) NOT NULL,
                                 with_resource VARCHAR(1023) NOT NULL,
                                 utc DATETIME NOT NULL,
                                 change_by VARCHAR(3071),
                                 change_utc DATETIME,
                                 deleted INTEGER,
                                 subject VARCHAR(1023),
                                 thread VARCHAR(1023),
                                 crypt INTEGER,
                                 extra VARCHAR(32767),
                                 PRIMARY KEY(id));
CREATE INDEX IDX_archive_colls_prev_id ON archive_collections(prev_id);
CREATE INDEX IDX_archive_colls_next_id ON archive_collections(next_id);
CREATE INDEX IDX_archive_colls_us ON archive_collections(us);
CREATE INDEX IDX_archive_colls_with_server ON archive_collections(with_server);
CREATE INDEX IDX_archive_colls_with_user ON archive_collections(with_user);
CREATE INDEX IDX_archive_colls_with_resource ON archive_collections(with_resource);
CREATE INDEX IDX_archive_colls_utc ON archive_collections(utc);
CREATE INDEX IDX_archive_colls_change_utc ON archive_collections(change_utc);

CREATE TABLE archive_messages(id INTEGER NOT NULL,
                              coll_id INTEGER NOT NULL,
                              utc DATETIME NOT NULL,
                              dir INTEGER,
                              body VARCHAR(65535),
                              name VARCHAR(1023),
                              PRIMARY KEY(id));
CREATE INDEX IDX_archive_msgs_coll_id ON archive_messages(coll_id);
CREATE INDEX IDX_archive_msgs_utc ON archive_messages(utc);

CREATE TABLE archive_jid_prefs(us VARCHAR(2047) NOT NULL,
                               with_user VARCHAR(1023) NOT NULL,
                               with_server VARCHAR(1023) NOT NULL,
                               with_resource VARCHAR(1023) NOT NULL,
                               save INTEGER,
                               expire INTEGER,
                               otr INTEGER,
                               PRIMARY KEY(us, with_user, with_server, with_resource));
CREATE INDEX IDX_archive_jid_prefs_us ON archive_jid_prefs(us);

CREATE TABLE archive_global_prefs(us VARCHAR(2047) NOT NULL,
                                  save INTEGER,
                                  expire INTEGER,
                                  otr INTEGER,
                                  method_auto INTEGER,
                                  method_local INTEGER,
                                  method_manual INTEGER,
                                  auto_save INTEGER,
                                  PRIMARY KEY(us));

CREATE TRIGGER archive_collections_delete BEFORE DELETE ON archive_collections
FOR EACH ROW
BEGIN
  DELETE FROM archive_messages WHERE coll_id = OLD.id;
  UPDATE archive_collections SET prev_id = null WHERE prev_id = OLD.id;
  UPDATE archive_collections SET next_id = null WHERE next_id = OLD.id;
END;

CREATE TRIGGER archive_collections_update BEFORE UPDATE ON archive_collections
FOR EACH ROW WHEN NEW.deleted = 1
BEGIN
  DELETE FROM archive_messages WHERE coll_id = NEW.id;
  UPDATE archive_collections SET prev_id = null WHERE prev_id = NEW.id;
  UPDATE archive_collections SET next_id = null WHERE next_id = NEW.id;
END;