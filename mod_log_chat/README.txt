mod_log_chat
============

mod_log_chat is an ejabberd module aimed at logging chat messages in
text files. mod_log_chat creates one file per couple of chatters and
per day (it doesn't log muc messages, use mod_muc_log for this).

It can store messages in plain text or HTML format.

Be sure that the directories where you want to create your log
files exists and are writable by you ejabberd user.
