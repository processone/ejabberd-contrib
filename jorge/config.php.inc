<?
/*
Jorge - frontend for mod_logdb - ejabberd server-side message archive module.

Copyright (C) 2009 Zbigniew Zolkiewski

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

*/

// Print debuging information (true|false). Do NOT set as true on producion servers!
define("DEBUG", false);
define("SQL_DEBUG",false); // sql debug

// Language support. Add your language below. DO NOT change IDs after jorge usage as this IDs are held in jorge_pref table.
$language_support = array(

	"Polski"=> array ("pol","1"),
	"English"=> array("eng", "2")

	);

// Define default language. Should be the name from language_support array.
define(default_language, "English");

// vhost and RPC settings
// RPC port:
$rpc_port="4666";

// vhost settings, format "xmpp.server.com"=>array of RPC servers (WARNING! Do not mistake vhost and RPC servers as this is crutial for authentication!)
$vhosts = array(

	"jabber.example.com"=>
			array("10.0.0.1","10.0.0.2"),

	"jabber.example.eu"=>
			array("10.0.0.3","10.0.0.4")
			
			);

// array of admins for domain
$vhosts_admins = array(

	"jabber.example.com"=> array("admin1","admin2","admin3"),
	"jabber.example.eu"=> array("admin10","admin20")

	);

// MySQL database where mod_logdb is running on:
define("MYSQL_USER", ""); // username
define("MYSQL_PASS", ""); // password
define("MYSQL_NAME",""); // db name
define("MYSQL_HOST", ""); // host ip

// Since version 1.5 Jorge is using reCAPTCHA code. Before running Jorge please go to: http://recaptcha.net/ and setup your account and get private key and public key:
define("CAPTCHA_PRIVATE","");
define("CAPTCHA_PUBLIC","");

// secret key for scrambling URLs. Put here some random data (32 chars):
define("ENC_KEY","PleaseChangeMe");

// Turn SSL redirection in PHP
define("SSL_REDIRECT", "false");

// number of chat lines in browser (default: 300)
$num_lines_bro = "300";

// number of search results (default: 100)
$num_search_results = "100";

// splitting line. Value in seconds. Default 900s = 15 minutes
$split_line="900";

// links
$links='<a class="foot" href="http://www.jabster.pl" target="_blank">jabster.pl</a>&nbsp;&nbsp;&nbsp;&nbsp;
	<a class="foot" href="http://poczta.jabster.pl" target="_blank">poczta</a>&nbsp;&nbsp;&nbsp;&nbsp;
	<a class="foot" href="http://kalendarz.jabster.pl" target="_blank">kalendarz</a>&nbsp;&nbsp;&nbsp;&nbsp;
	<a class="foot" href="http://docs.jabster.pl" target="_blank">dokumenty</a>
	'; 

// copyright
$copy = "jabster.pl &copy; 2009";

// custom logo
$brand_logo = "logo_jabster.png";

?>
