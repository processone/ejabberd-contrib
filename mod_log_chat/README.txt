mod_log_chat
============

mod_log_chat is an ejabberd module aimed at logging chat messages in
text files. mod_log_chat creates one file per couple of chatters and
per day (it doesn't log muc messages, use mod_muc_log for this).

It can store messages in plain text or HTML format.

Compilation and installation
----------------------------

- You need to have Erlang installed.

- Run
  erl -pa ../ejabberd-dev/ebin -make
  in the mod_log_chat directory.

- Copy generated mod_log_chat.beam file from the ebin directory to the
  directory where your ejabberd .beam files are.

- Edit the "modules" section of your ejabberd.cfg configuration file to
  suit your needs (see conf/ejabberd.conf.sample for examples).

- Be sure that the directories where you want to create your log
  files exists and are writable by you ejabberd user.

- Restart ejabberd.
