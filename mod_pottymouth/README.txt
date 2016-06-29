The 'mod_pottymouth' ejabberd module aims to fill the void left by 'mod_shit'
which has disappeared from the net. It allows individual whole words of a
message to be filtered against a blacklist. It allows multiple blacklists
sharded by language. To make use of this module the client must add the xml:lang
attribute to the message xml.

To install in ejabberd:

cd ~/.ejabberd-modules/sources
clone the git repo
cd mod_pottymouth
edit: ./conf/mod_pottymouth.yml

make sure ejabberd is running
run: ejabberdctl module_install mod_pottymouth
run: ejabberdctl restart
module will be installed in: ~/.ejabberd-modules/mod_pottymouth

Config file format:

modules:
    mod_pottymouth:
        blacklists:
            default: /home/your_user/blacklist_en.txt
            en: /home/your_user/blacklist_en.txt
            cn: /home/your_user/blacklist_cn.txt
            fr: /home/your_user/blacklist_fr.txt

For each language (en,cn,fr,...whatever) provide a full path to a backlist file.
The blacklist file is a plain text file with blacklisted words listed one per
line.

Gotchas:

The language will be looked up by whatever value is passed in the xml:lang
attribute of the xml message. So, any xml:lang value to be supported will need
a corresponding entry/blacklist in the config file. If xml:lang is missing,
the 'default' entry in config will be used.

For xml:lang attribute docs, see:
http://wiki.xmpp.org/web/Programming_XMPP_Clients#Sending_a_message

The internal bloomfilter used to ingest the blacklists currently requires about
4,000 entries in the blacklist to ensure acceptable error probability. (We've
gotten around this by duplicating entries in a short list)

Todo:

Look into acceptable error probabilities for shorter blacklists.

Tip of the hat:

This mod makes use of the excellent 'etbloom' module:
https://github.com/erlangtoolbox/etbloom
