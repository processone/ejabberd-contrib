CREATE SEQUENCE archive_collection_sid_seq;

CREATE TABLE archive_collection (
    sid bigint NOT NULL DEFAULT nextval('archive_collection_sid_seq') PRIMARY KEY,
    username text NOT NULL,
    jid_u text NOT NULL,
    jid_s text NOT NULL,
    jid_r text NOT NULL,
    start int8 NOT NULL,
    subject text NOT NULL
);

CREATE INDEX i_archive_collection_username ON archive_collection USING btree (username);
CREATE INDEX i_archive_collection_user_jid ON archive_collection USING btree (username, jid_u, jid_s);
CREATE UNIQUE INDEX i_archive_collection_user_jid_start ON archive_collection USING btree (username, jid_u, jid_s, start);

CREATE SEQUENCE archive_message_ser_seq;

CREATE TABLE archive_message (
    sid bigint REFERENCES archive_collection ON DELETE CASCADE,
    ser bigint NOT NULL DEFAULT nextval('archive_message_ser_seq'),
    direction_to boolean NOT NULL,
    secs int NOT NULL,
    utc int8 NOT NULL,
    name text NOT NULL,
    jid text NOT NULL,
    body text NOT NULL
);

CREATE INDEX i_archive_message_sid ON archive_message USING btree (sid);

-- FTS support
-- Requires tsearch2 and btree_gist

ALTER TABLE archive_collection ADD COLUMN fts tsvector NOT NULL DEFAULT to_tsvector('');

CREATE INDEX i_archive_collection_fts on archive_collection USING gist(username, fts);

CREATE RULE r_archive_fts_update AS ON INSERT TO archive_message DO ALSO
    UPDATE archive_collection
        SET fts = fts || to_tsvector(translate(NEW.body, '<>&', '   '))
        WHERE sid = NEW.sid;

--CREATE TABLE archive_fts (
--    sid bigint PRIMARY KEY REFERENCES archive_collection ON DELETE CASCADE,
--    username text NOT NULL,
--    fts tsvector
--);
--
--CREATE INDEX i_archive_fts on archive_fts USING gist(username, fts);
--
--CREATE RULE r_archive_fts_empty AS ON INSERT TO archive_collection DO ALSO
--    INSERT INTO archive_fts(sid, username, fts)
--        VALUES (NEW.sid, NEW.username, to_tsvector(''));
--
--CREATE RULE r_archive_fts_update AS ON INSERT TO archive_message DO ALSO
--    UPDATE archive_fts
--        SET fts = fts || to_tsvector(translate(NEW.body, '<>&', '   '))
--        WHERE sid = NEW.sid;

