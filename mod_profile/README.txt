
	mod_profile - User Profile (XEP-0154) in Mnesia table 

	http://www.ejabberd.im/mod_profile
	Author: Magnus Henoch
	    mailto:henoch@dtek.chalmers.se
	    xmpp:legoscia@jabber.cd.chalmers.se
	Requirements: ejabberd 2.x.x


This module supports storing and retrieving a profile according to
XEP-0154.  It does no validation of the data, but simply stores
whatever XML the user sends in a Mnesia table.  The PEP parts of
XEP-0154 are out of scope for this module.

In the beginning of the erl file it says what parts of the XEP
are implemented.


	BASIC CONFIGURATION
	===================

To install this module, follow the general build instructions, and add the
following to your configuration, among the other modules:

{mod_profile, []}

