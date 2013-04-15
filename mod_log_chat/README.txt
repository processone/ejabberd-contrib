mod_log_chat
============

mod_log_chat is a ejabberd module aimed at logging chat messages in
text files. mod_log_chat create one file per couple of chatters and
per day (it doesn't log muc messages, use mod_muc_log for this).

It can store messages in plain text or HTML format.

Compilation and installation
----------------------------

You need to have Erlang installed as well as the ejabberd-dev module
(checkout it in the same directory than mod_log_chat is).

- Run
  erl -pa ../../ejabberd-dev/trunk/ebin -make
  in the trunk directory of mod_log_chat.

- Copy generated mod_log_chat.beam file from the ebin directory to the
  directory where your ejabberd .beam files are.

- Edit the "modules" section of your ejabberd.cfg configuration file to
  suit your needs (see conf/ejabberd.conf.sample for examples).

- Be sure that the directories where you want to create your log
  files exists and are writable by you ejabberd user.

- Restart ejabberd.
