
	mod_s2s_log
	-----------

	Requirements: ejabberd SVN r1235 trunk or 2.0.x branch
	   (ejabberd 2.0.1 or newer is required)


This module can be use to keep a track of other XMPP servers your server has
been connected with.

You can use it by adding the following line in the  modules section of
ejabberd.cfg:
       {mod_s2s_log, [{filename, "/path/to/s2s.log"}]}
