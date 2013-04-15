DROP TABLE archive_collections;

CREATE TABLE archive_collections(id SERIAL not null,
                                 prev_id INTEGER,
                                 next_id INTEGER,
                                 us VARCHAR(2047) NOT NULL,
                                 with_user VARCHAR(1023) NOT NULL,
                                 with_server VARCHAR(1023) NOT NULL,
                                 with_resource VARCHAR(1023) NOT NULL,
                                 utc timestamp NOT NULL,
                                 change_by VARCHAR(3071),
                                 change_utc timestamp,
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

DROP TABLE archive_messages;
CREATE TABLE archive_messages(id SERIAL NOT NULL,
                              coll_id INTEGER NOT NULL,
                              utc timestamp NOT NULL,
                              dir INTEGER,
                              body VARCHAR(65535),
                              name VARCHAR(1023),
                              PRIMARY KEY(id));
CREATE INDEX IDX_archive_msgs_coll_id ON archive_messages(coll_id);
CREATE INDEX IDX_archive_msgs_utc ON archive_messages(utc);

DROP TABLE archive_jid_prefs;
CREATE TABLE archive_jid_prefs(us VARCHAR(2047) NOT NULL,
                               with_user VARCHAR(1023) NOT NULL,
                               with_server VARCHAR(1023) NOT NULL,
                               with_resource VARCHAR(1023) NOT NULL,
                               save integer,
                               expire INTEGER,
                               otr integer);
CREATE INDEX IDX_archive_jid_prefs_us ON archive_jid_prefs(us);
CREATE INDEX IDX_archive_jid_prefs_with_user ON archive_jid_prefs(with_user);
CREATE INDEX IDX_archive_jid_prefs_with_server ON archive_jid_prefs(with_server);
CREATE INDEX IDX_archive_jid_prefs_with_resource ON archive_jid_prefs(with_resource);

DROP TABLE archive_global_prefs;
CREATE TABLE archive_global_prefs(us VARCHAR(2047) NOT NULL,
                                  save integer,
                                  expire INTEGER,
                                  otr integer,
                                  method_auto integer,
                                  method_local integer,
                                  method_manual integer,
                                  auto_save integer);
CREATE INDEX IDX_archive_global_prefs_us ON archive_global_prefs(us);


CREATE RULE archive_collections_delete AS ON DELETE 
    TO archive_collections
    DO DELETE FROM archive_messages WHERE coll_id = OLD.id;
    
CREATE RULE archive_collections_update AS ON UPDATE
    TO archive_collections
    DO DELETE FROM archive_messages WHERE coll_id = NEW.id and NEW.deleted=1;
    

