The 'mod_pottymouth' ejabberd module aims to fill the void left by 'mod_shit'
which has disappeared from the net. It allows individual whole words of a
message to be filtered against a blacklist. It allows multiple blacklists
sharded by language. The internal bloomfilter can support arbitrary blacklist
sizes. Using a large list (say, 87M terms) will slow down the initial server
boot time (to about 15 minutes respectively), but once loaded lookups are very
speedy.

Prerequisite bitarray lib:

mod_pottymouth uses a modified version of the 'etbloom' library that uses 
'bitarray' to replace 'hipe_bifs'. Ejabberd doesn't handle installing 
dependences of dependecies quite so well (etbloom being a dep of mod_pottymouth 
and bitarray being a dep of etbloom), so bitarray needs to be installed manually 
before installation of mod_pottymouth.

This is how I got it to work... YMMV.

1. Make sure ejabberd is running
2. Get the updated ejabberd-contrib sources:
   ejabberdctl modules_update_specs
3. Then get manually the dependencies, as the scripts can't do it:
   cd ~/.ejabberd-modules/sources/ejabberd-contrib/mod_pottymouth/deps/etbloom/
   chmod +x rebar
   ./rebar get-deps
   ./rebar compile
   cp -R deps/bitarray/ebin ../../
   cp -R deps/bitarray/priv ../../
   cp -R deps/proper/ebin ../../
4. Finally, when you install the module, its dependencies will be installed as well:
   ejabberdctl module_install mod_pottymouth

Configuration file is ~/.ejabberd-modules/mod_pottymouth/conf/mod_pottymouth.yml

modules:
    mod_pottymouth:
        blacklists:
            default: /home/your_user/blacklist_en.txt
            en: /home/your_user/blacklist_en.txt
            cn: /home/your_user/blacklist_cn.txt
            fr: /home/your_user/blacklist_fr.txt
        charmaps:
            default: /etc/ejabberd/modules/mod_pottymouth/charmap_en.txt
            en: /etc/ejabberd/modules/mod_pottymouth/charmap_en.txt

For each language (en,cn,fr,...whatever) provide a full path to a backlist file.
The blacklist file is a plain text file with blacklisted words listed one per
line.

You can also provide an optional 'charmap' for each language. This allows you
to specify simple substitutions that will be made on the fly so you don't need
to include those permutations in the blacklist. This keeps the blacklist small
and reduces server startup time. For example, if you included the word:
'xyza' in the blacklist, adding the following substitutions in the charmap
would filter permutations such as 'XYZA', 'xYz4', or 'Xyz@' automatically.

charmap format:

[
 {"X", "x"},
 {"Y", "y"},
 {"Z", "z"},
 {"@", "a"},
 {"4", "a"}
].

Gotchas:

The language will be looked up by whatever value is passed in the xml:lang
attribute of the xml message. So, any xml:lang value to be supported will need
a corresponding entry/blacklist in the config file. If xml:lang is missing,
the 'default' entry in config will be used.

For xml:lang attribute docs, see:
http://wiki.xmpp.org/web/Programming_XMPP_Clients#Sending_a_message

Blacklist helper

Thinking of a bunch of swear words and all the permutations can be tough. We made
a helper script to take a bare wordlist and generate permutations given a
dictionary of substitution characters: https://github.com/madglory/permute_wordlist

Tip of the hat:

This mod makes use of the excellent 'etbloom' module:
https://github.com/erlangtoolbox/etbloom
