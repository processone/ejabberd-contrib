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

if (__FILE__==$_SERVER['SCRIPT_FILENAME']) {

	header("Location: index.php?act=logout");
	exit;

}

// turn on buffering
ob_start();
// send headers
header("content-type: text/html; charset=utf-8");

// error reporting to off
error_reporting(E_NONE);

require_once("func.php"); // functions
require_once("class.sessions.php"); // sessions handling
require_once("class.ejabberd_xmlrpc.php"); // rpc class
require_once("class.db.php"); // db_manager
require_once("class.roster.php"); // roster
require_once("class.helper.php"); // helper
require_once("config.php"); // read configuration

# SSL redirection if set to true in config.
if (SSL_REDIRECT === "true") {

	if($_SERVER['HTTPS'] != 'on') {

		$url = "https://".$_SERVER['HTTP_HOST'].$_SERVER['REQUEST_URI'];
	        header('Location: '.$url); 
		ob_end_flush();
		exit;
	}
}

$sess = new session;
// Language support. Well thats the hard way...
$language_found = false;

if ($_GET['lng_sw']) {

		debug(DEBUG,"Setting language");
		$c_language = is_language_supported($_GET['lng_sw'],$language_support,1,true);
		if ($c_language !== false) {

				setcookie("jorge_language",$c_language,time()+2592000);
				$sess->set('language',$c_language);
				debug(DEBUG,"Language found, loading file: $c_language.php");
				require("lang/".$c_language.".php");
				$language_found = true;
			
			}
			else{
			
				setcookie("jorge_language",$language_support[default_language][0],time()+2592000);
				$sess->set('language',$language_support[default_language][0]);
				debug(DEBUG,"Language not found in selection, using defaults");
				require('lang/'.$language_support[default_language][0].'.php');
		}

}
								
// get client addr
$rem_adre = $_SERVER['REMOTE_ADDR'];

// something for mac users
$mac_user = get_user_agent($_SERVER);

// location
$location=$_SERVER['PHP_SELF'];

// init html helper
$html = new render_html();

if (!preg_match("/index.php/i",$location)) {

		if ($sess->get('vhost') === null) {

			header("Location: index.php?act=logout");
		
		}
		if (array_key_exists($sess->get('vhost'), $vhosts) === false) {

			header("Location: index.php?act=logout");
			
		}

		define(XMPP_HOST,$sess->get('vhost'));
		$rpc_host = check_rpc_server($vhosts[XMPP_HOST],$rpc_port);
		debug(DEBUG,"Active RPC host: $rpc_host");

		// in case no RPC servers are available stop jorge
		if ($rpc_host===false) {

			print "<br><center><b>Currently service is unavailable. Please try again later.</b><br>
				<a href=\"index.php?act=logout\">Please logout</a>
			</center>";
			exit;
		}

		// create rpc object
		$ejabberd_rpc = new rpc_connector("$rpc_host","$rpc_port",XMPP_HOST);
		$xmpp_host = str_replace(".","_", XMPP_HOST);

	}
	else{

		// check if selected host exist in configuration
		if (array_key_exists($_POST['vhost'], $vhosts) === true) {
	
				$rpc_host = check_rpc_server($vhosts[$_POST['vhost']],$rpc_port);
				debug(DEBUG,"Selecting RPC server during login: $rpc_host");
				if ($rpc_host === false) {

						print "<br><center><b>Currently service is unavailable. Please try again later.<br>
							<a href=\"index.php?act=logout\">Please logout</a>
						</b></center>";
						exit;
					
					}
					else {

						define(XMPP_HOST,$_POST['vhost']);		
						$ejabberd_rpc = new rpc_connector("$rpc_host","$rpc_port",XMPP_HOST);
						$xmpp_host = str_replace(".","_", XMPP_HOST);

				}

			}
			else{

				unset($_POST['inpLogin']);
				unset($_POST['inpPass']);

				// Try to recreate db object...
				if ($_GET['act'] === "logout") {

					if ($sess->get('vhost')!="") {

						$xmpp_host = str_replace(".","_", $sess->get('vhost'));
					
					}
				}

		}

}

// create db_manager object
$db = new db_manager(MYSQL_HOST,MYSQL_NAME,MYSQL_USER,MYSQL_PASS,"mysql","$xmpp_host");
$db->set_debug(SQL_DEBUG);

// create encryption object
$enc = new url_crypt(ENC_KEY);

// username (token)
define(TOKEN,$sess->get('uid_l'));

//debug
debug(DEBUG,"User session:".TOKEN);

// authentication checks. Ensure if session data is not altered... (only when we are inside Jorge)
if (!preg_match("/index.php/i",$location)) {

	if (check_registered_user($sess,$ejabberd_rpc,$enc) !== true) { 

			header("Location: index.php?act=logout"); 
			exit; 
	}

	// Load language file based on current session
	debug(DEBUG,"Selecting initial language after authentication");
	if ($sess->get('language')) {

			// Validate language setting in session
			if (is_language_supported($sess->get('language'),$language_support) === true) {

					debug(DEBUG,"Language selection ok.");
					require('lang/'.$sess->get('language').'.php');

				}
				else{

					debug(DEBUG,"Language in session was altered! Overwritting value...");
					require('lang/'.$language_support[default_language][0].'.php');
					$sess->set('language',$language_support[default_language][0]);

			}
	}

	// we need user_id but only if we are not in not_enabled mode:
	if(!preg_match("/not_enabled.php/i",$_SERVER['PHP_SELF'])) {

		$db->get_user_id(TOKEN);
		$user_id = $db->result->user_id;
		// create user_id instance
		$db->set_user_id($user_id);
	}

}

// check if user have admin rights
if (in_array(TOKEN, $vhosts_admins[XMPP_HOST]) === true) {

	define(ADMIN_NAME,TOKEN);

}

// run only for admins
if (TOKEN === ADMIN_NAME) {

		$time_start=getmicrotime();

}

// If language not set or not found in cookie, set default language
if (preg_match("/index.php/i",$location) OR preg_match("/not_enabled.php/i",$location)) {

		// Set defaults only if language was not selected
		if ($language_found !== true) {

			if (is_language_supported($_COOKIE["jorge_language"],$language_support) === true) {

					debug(DEBUG,"Setting language according to cookie");
					require('lang/'.$_COOKIE["jorge_language"].'.php');
					$sess->set('language',$_COOKIE["jorge_language"]);

				}
				else {

					debug(DEBUG,"Language cookie not found, using defaults");
					require('lang/'.$language_support[default_language][0].'.php');
					$sess->set('language',$language_support[default_language][0]);

			}


		}

}

// Get language from session
$lang=$sess->get('language');

$html->headers('
		<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
		<html>
		<head>
			<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
			<meta http-equiv="cache-control" content="no-cache">
			<meta http-equiv="pragma" content="no-cache">
			<meta name="Author" content="Zbyszek Zolkiewski at jabster.pl">
			<meta name="Keywords" content="jorge message archiving ejabberd mod_logdb erlang">
			<meta name="Description" content="Jorge">
			<link rel="shortcut icon" href="favicon.ico"> 
			<link rel="stylesheet" href="style.css" type="text/css">
			<link rel="stylesheet" href="jquery.autocomplete.css" type="text/css">
			<link rel="alternate" type="application/rss+xml" title="Project Jorge ChangeLog" href="https://jorge.jabster.pl/changelog.xml">

	');

if (preg_match("/main.php/i",$location)) {

	$html->headers('
			<link rel="stylesheet" href="simpletree.css" type="text/css" />
			<script type="text/javascript" src="lib/simpletreemenu.js">
				/***********************************************
				* Simple Tree Menu - Dynamic Drive DHTML code library (www.dynamicdrive.com)
				* This notice MUST stay intact for legal use
				* Visit Dynamic Drive at http://www.dynamicdrive.com/ for full source code
				***********************************************/	
			</script>
	');
}
$html->headers('
        		<script type="text/javascript" src="lib/jquery.pack.js"></script>
			<script type="text/javascript" src="lib/jquery.bgiframe.min.js"></script>
			<script type="text/javascript" src="lib/jquery.tooltip.js"></script>
			<script type="text/javascript" src="lib/jquery.quicksearch.js"></script>
			<script type="text/javascript" src="lib/jquery.autocomplete.pack.js"></script>
	
	');

// prevent loading includes as long as user is not admin.
if (TOKEN==ADMIN_NAME) {

	$html->headers('<script language="javascript" type="text/javascript" src="lib/jquery.flot.pack.js"></script>');
}
$html->headers('
	<title>Jorge Beta</title>
        <script type="text/javascript">
            $(function() {
		$(\'table#maincontent tbody#searchfield tr\').quicksearch({
			stripeRowClass: [\'odd\', \'even\'],
			position: \'before\',
			attached: \'#maincontent\',
			labelText: \'QuickFilter:\',
			loaderText: \'\',
			inputClass: \'c_map_class\',
			delay: 30
		});


	$(\'select#c_map option\').quicksearch({
			stripeRowClass: [\'odd\', \'even\'],
			position: \'before\',
			inputText: \''.$filter_form_tip[$lang].'\',
			inputClass: \'c_map_class\',
			attached: \'#c_map_form\',
			labelText: \''.$filter_form[$lang].':\',
			delay: 30
		});


		$(\'img\').Tooltip();

		$(\'a, tr, td\').Tooltip({
			extraClass: "fancy",
			showBody: ";",
			showURL: false,
			track: true,
			fixPNG: true
		});


            });
	</script>
	</head>
	<body style="background-image: url(img/bak2b.png); background-repeat:repeat-x; background-color: #edf5fa;">

	<noscript>
 
		<center><div style="background-color: #fad163; text-align: center; font-weight: bold; width: 500pt;">'.$no_script[$lang].'</div></center><br>

	</noscript>

	<script language="JavaScript1.2" type="text/javascript">

	function smackzk()  {

		window.open(\'http://slimster.org/web\',\'\',
			\'location=no,toolbar=no,menubar=no,scrollbars=no,resizable, height=375,width=715\');

		}
	</script>

	');

?>

