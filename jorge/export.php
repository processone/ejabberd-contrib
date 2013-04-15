<?php
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

// Suppress all errors
error_reporting(E_NONE);

require("func.php");
require("class.sessions.php");
require("class.ejabberd_xmlrpc.php");
require("class.db.php");
require("class.roster.php");
require("class.helper.php");
require("config.php");

ob_start();

// init session
$sess = new session;

// Select language
if ($sess->get('language')) {

			// Validate language setting in session
			if (is_language_supported($sess->get('language'),$language_support) === true) {

					require('lang/'.$sess->get('language').'.php');

				}
				else{

					// In case of invalid session, overwrite value
					require('lang/'.$language_support[default_language][0].'.php');
					$sess->set('language',$language_support[default_language][0]);

			}
	}
	else{

			// If no lang in sess, set it anyway...
			require('lang/'.$language_support[default_language][0].'.php');
			$sess->set('language',$language_support[default_language][0]);

}

// language
$lang = $sess->get('language');

define(XMPP_HOST,$sess->get('vhost'));
$xmpp_host = str_replace(".","_", XMPP_HOST);

$rpc_host = check_rpc_server($vhosts[XMPP_HOST],$rpc_port);

// in case no RPC servers are available stop jorge
if ($rpc_host===false) {

		print "<br><center><b>Currently service is unavailable. Please try again later.</b></center>";
		exit;
	}

// connect to xmpp server
$ejabberd_rpc = new rpc_connector("$rpc_host","$rpc_port",XMPP_HOST);

// initialize encryption system
$enc = new url_crypt(ENC_KEY);

// authenticate
if (check_registered_user($sess,$ejabberd_rpc,$enc) !== true) { 

	header("Location: index.php?act=logout"); 
	exit; 

}

// create database object
$db = new db_manager(MYSQL_HOST,MYSQL_NAME,MYSQL_USER,MYSQL_PASS,"mysql","$xmpp_host");

// set user data
define(TOKEN,$sess->get('uid_l'));
$db->get_user_id(TOKEN);
define(USER_ID, $db->result->user_id);

if (!ctype_digit(USER_ID)) { 

		// exit on unexpected results
		exit; 

	}
	else{

		$db->set_user_id(USER_ID);
	
}

// get parameters
$e_string=$_GET['a'];


if ($enc->decrypt_url($e_string) === true) {

		$tslice = $enc->tslice;
		$talker = $enc->peer_name_id;
		$server = $enc->peer_server_id;

	}	
	else {

		// if validation of link fail, exit
		header('Location: index.php');
		exit;

}

$db->get_user_name($talker);
$user_name = $db->result->username;
$db->get_server_name($server);
$server_name = $db->result->server_name;
$nickname = $sess->get('export_nickname');
$db->get_own_name();
if ($db->result->own_name) {

		$own_name = $db->result->own_name;

	}
	else{

		$own_name = false;

}

// get chat
$db->get_user_chat($tslice,$talker,$server,$resource_id = null,$start = null,10000);
$result = $db->result;

// set headers
header("Cache-Control: public, must-revalidate");
header("Pragma: hack"); // this is WEIRD - it is needed to work with Internet Explorer :O
header("Content-Type: application/octet-stream");
header("Content-Disposition: attachment; filename=\"Jorge_chat_$nickname-$tslice.txt\"");

$data="$export_head1[$lang]$nickname ($user_name@$server_name) $export_head2[$lang] $tslice:\n";

foreach ($result as $results) {

	if ($results["direction"] == "from") 
				{ 
					$out=$nickname;
					$tt=$tt+1;
					$aa=0;
				} 
				else 
				{ 
					if ($own_name !== false) {

							$out = $own_name;
						}
						else{

							$out = TOKEN;

					}
					$aa=$aa+1;
					$tt=0;
				}


			if ($aa<2 AND $tt<2) {
			
					$data .= "\n(".trim(strstr($results[ts], ' ')).") $out\n"; 
					$here="1"; 
				} 
				else 
				{ 
					$data .=""; $here="0"; 
				}

	$data .="       $results[body]\n";

}

$data .="\n\n______\nChat exported by Jorge";

echo $data;
ob_end_flush();

// log event
$db->set_logger("8","1", "JID: $user_name@$server_name, Date: $tslice");
$sess->unregister('export_nickname');

?>
