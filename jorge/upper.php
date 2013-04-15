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

// If user have profile
if ($sess->get('log_status') === null) { 

	header ("Location: not_enabled.php"); 
	exit;
	
}

// number of my links saved...
$db->get_mylinks_count();
$my_links_count = $db->result->cnt;

// number of items in trash
$db->get_trash_count();
$tr_n = $db->result->cnt;

// number of favorites
$db->get_favorites_count();
$favorites_count = $db->result->cnt;

// get preferences for saving
$pref_id=$_GET['set_pref'];
$pref_value=$_GET['v'];

// save preferences ONLY. Setting language in session is done in headers, here we only save that preferences.
if ($_GET['set_pref']) {

	// Language selection
	if ($pref_id === "2") {

		//Rewrite array for late reuse
		$language_change = $language_support;
		// Here the $pref_value is actually $lng_sw
		$pref_value = $_GET['lng_sw'];
		while(array_keys($language_change)) {

			$lang_key = key($language_change);
			if (in_array($pref_value,$language_change[$lang_key])) {

				debug(DEBUG,"Saving language preferences into database...");
				if ($db->set_jorge_pref($pref_id,$pref_value) === false) {

						$html->alert_message($oper_fail[$lang],"message");
						debug(DEBUG,"Preferences not saved due to error");

					}
					else{

						$html->status_message($con_saved[$lang]);
						debug(DEBUG,"Preferences saved successfuly");

				}

				break 1;

			}

			array_shift($language_change);

		}
	}

	// default view type
	if ($pref_id ==="1") { 

		debug(DEBUG,"Saving view selection into database");
		if($pref_value === "1" OR $pref_value === "2") { 

				if ($db->set_jorge_pref($pref_id,$pref_value) === false) {

						$html->alert_message($oper_fail[$lang],"message");
						debug(DEBUG,"Preferences not saved due to error");
						
					}
					else{

						// Display status message only if setting via control panel
						if ($_GET['ref'] === "settings") {

							$html->status_message($con_saved[$lang]);
							debug(DEBUG,"Preferences saved successfuly");

						}

				}

			$sess->set('view_type',$pref_value);

		} 
	}

}

// get preferences, if not set, fallback to standard view.
$view_type=$sess->get('view_type');
if ($view_type=="1") { 

		$view_type="main.php"; 
	} 
	elseif($view_type=="2") { 
	
		$view_type="calendar_view.php"; 
		
}

// this is menu. not nice but works ;)
if (preg_match("/search_v2.php/i",$location)) 

	{ 
		$menu_main='<a class="mmenu" href="'.$view_type.'">'.$menu_item_browser[$lang].'</a>';
		$menu_map='<a class="mmenu" href="chat_map.php">'.$menu_item_map[$lang].'</a>';
		$menu_search='<b>'.$menu_item_search[$lang].'</b>';
		$menu_mylinks='<a class="mmenu" href="my_links.php">'.$menu_item_links[$lang].' ('.$my_links_count.')</a>';
		$menu_favorites='<a class="mmenu" href="favorites.php">'.$menu_item_fav[$lang].'('.$favorites_count.')</a>';
		$menu_contacts='<a class="mmenu" href="contacts.php">'.$menu_item_contacts[$lang].'</a>';
		$menu_logger='<a class="mmenu" href="logger.php">'.$menu_item_logs[$lang].'</a>';
		$menu_trash='<a class="mmenu" href="trash.php">'.$menu_item_trash[$lang].'('.$tr_n.')</a>';
		$search_loc=1;
		if (TOKEN==ADMIN_NAME) { $menu_stats=' | <a class="mmenu" href="stats.php"> Stats</a>'; }
	}
	elseif(preg_match("/main.php/i",$location))
	{
		$menu_main='<b>'.$menu_item_browser[$lang].'</b>';
		$menu_map='<a class="mmenu" href="chat_map.php">'.$menu_item_map[$lang].'</a>';
		$menu_search='<a class="mmenu" href="search_v2.php">'.$menu_item_search[$lang].'</a>';
		$menu_mylinks='<a class="mmenu" href="my_links.php">'.$menu_item_links[$lang].' ('.$my_links_count.')</a>';
		$menu_favorites='<a class="mmenu" href="favorites.php">'.$menu_item_fav[$lang].'('.$favorites_count.')</a>';
		$menu_contacts='<a class="mmenu" href="contacts.php">'.$menu_item_contacts[$lang].'</a>';
		$menu_logger='<a class="mmenu" href="logger.php">'.$menu_item_logs[$lang].'</a>';
		$menu_trash='<a class="mmenu" href="trash.php">'.$menu_item_trash[$lang].'('.$tr_n.')</a>';
		if (TOKEN==ADMIN_NAME) { $menu_stats=' | <a class="mmenu" href="stats.php"> Stats</a>'; }

	}
	elseif(preg_match("/my_links.php/i",$location))
	{
		$menu_main='<a class="mmenu" href="'.$view_type.'">'.$menu_item_browser[$lang].'</a>';
		$menu_map='<a class="mmenu" href="chat_map.php">'.$menu_item_map[$lang].'</a>';
		$menu_search='<a class="mmenu" href="search_v2.php">'.$menu_item_search[$lang].'</a>';
		$menu_mylinks='<b>'.$menu_item_links[$lang].' ('.$my_links_count.') </b>';
		$menu_favorites='<a class="mmenu" href="favorites.php">'.$menu_item_fav[$lang].'('.$favorites_count.')</a>';
		$menu_contacts='<a class="mmenu" href="contacts.php">'.$menu_item_contacts[$lang].'</a>';
		$menu_logger='<a class="mmenu" href="logger.php">'.$menu_item_logs[$lang].'</a>';
		$menu_trash='<a class="mmenu" href="trash.php">'.$menu_item_trash[$lang].'('.$tr_n.')</a>';
		if (TOKEN==ADMIN_NAME) { $menu_stats=' | <a class="mmenu" href="stats.php"> Stats</a>'; }


	}
	elseif(preg_match("/help.php/i",$location))
	{
		$menu_main='<a class="mmenu" href="'.$view_type.'">'.$menu_item_browser[$lang].'</a>';
		$menu_map='<a class="mmenu" href="chat_map.php">'.$menu_item_map[$lang].'</a>';
		$menu_search='<a class="mmenu" href="search_v2.php">'.$menu_item_search[$lang].'</a>';
		$menu_mylinks='<a class="mmenu" href="my_links.php">'.$menu_item_links[$lang].' ('.$my_links_count.')</a>';
		$menu_favorites='<a class="mmenu" href="favorites.php">'.$menu_item_fav[$lang].'('.$favorites_count.')</a>';
		$menu_contacts='<a class="mmenu" href="contacts.php">'.$menu_item_contacts[$lang].'</a>';
		$menu_logger='<a class="mmenu" href="logger.php">'.$menu_item_logs[$lang].'</a>';
		$menu_trash='<a class="mmenu" href="trash.php">'.$menu_item_trash[$lang].'('.$tr_n.')</a>';
		if (TOKEN==ADMIN_NAME) { $menu_stats=' | <a class="mmenu" href="stats.php"> Stats</a>'; }

	}
	elseif(preg_match("/contacts.php/i", $location))
	{
		$menu_main='<a class="mmenu" href="'.$view_type.'">'.$menu_item_browser[$lang].'</a>';
		$menu_map='<a class="mmenu" href="chat_map.php">'.$menu_item_map[$lang].'</a>';
		$menu_search='<a class="mmenu" href="search_v2.php">'.$menu_item_search[$lang].'</a>';
		$menu_mylinks='<a class="mmenu" href="my_links.php">'.$menu_item_links[$lang].' ('.$my_links_count.')</a>';
		$menu_favorites='<a class="mmenu" href="favorites.php">'.$menu_item_fav[$lang].'('.$favorites_count.')</a>';
		$menu_contacts='<b>'.$menu_item_contacts[$lang].'</b>';
		$menu_logger='<a class="mmenu" href="logger.php">'.$menu_item_logs[$lang].'</a>';
		$menu_trash='<a class="mmenu" href="trash.php">'.$menu_item_trash[$lang].'('.$tr_n.')</a>';
		if (TOKEN==ADMIN_NAME) { $menu_stats=' | <a class="mmenu" href="stats.php">Stats</a>'; }

	}
	elseif(preg_match("/stats.php/i", $location))
	{
		$menu_main='<a class="mmenu" href="'.$view_type.'">'.$menu_item_browser[$lang].'</a>';
		$menu_map='<a class="mmenu" href="chat_map.php">'.$menu_item_map[$lang].'</a>';
		$menu_search='<a class="mmenu" href="search_v2.php">'.$menu_item_search[$lang].'</a>';
		$menu_mylinks='<a class="mmenu" href="my_links.php">'.$menu_item_links[$lang].' ('.$my_links_count.')</a>';
		$menu_favorites='<a class="mmenu" href="favorites.php">'.$menu_item_fav[$lang].'('.$favorites_count.')</a>';
		$menu_contacts='<a class="mmenu" href="contacts.php">'.$menu_item_contacts[$lang].'</a>';
		$menu_logger='<a class="mmenu" href="logger.php">'.$menu_item_logs[$lang].'</a>';
		$menu_trash='<a class="mmenu" href="trash.php">'.$menu_item_trash[$lang].'('.$tr_n.')</a>';
		if (TOKEN==ADMIN_NAME) { $menu_stats=' | <b>Stats</b></a>'; }

		
	}
	elseif(preg_match("/logger.php/i", $location))
	{
		$menu_main='<a class="mmenu" href="'.$view_type.'">'.$menu_item_browser[$lang].'</a>';
		$menu_map='<a class="mmenu" href="chat_map.php">'.$menu_item_map[$lang].'</a>';
		$menu_search='<a class="mmenu" href="search_v2.php">'.$menu_item_search[$lang].'</a>';
		$menu_mylinks='<a class="mmenu" href="my_links.php">'.$menu_item_links[$lang].' ('.$my_links_count.')</a>';
		$menu_favorites='<a class="mmenu" href="favorites.php">'.$menu_item_fav[$lang].'('.$favorites_count.')</a>';
		$menu_contacts='<a class="mmenu" href="contacts.php">'.$menu_item_contacts[$lang].'</a>';
		$menu_logger='<b>'.$menu_item_logs[$lang].'</b>';
		$menu_trash='<a class="mmenu" href="trash.php">'.$menu_item_trash[$lang].'('.$tr_n.')</a>';
		if (TOKEN==ADMIN_NAME) { $menu_stats=' | <a class="mmenu" href="stats.php"> Stats</a>'; }

		
	}
	elseif(preg_match("/trash.php/i", $location))
	{
		$menu_main='<a class="mmenu" href="'.$view_type.'">'.$menu_item_browser[$lang].'</a>';
		$menu_map='<a class="mmenu" href="chat_map.php">'.$menu_item_map[$lang].'</a>';
		$menu_search='<a class="mmenu" href="search_v2.php">'.$menu_item_search[$lang].'</a>';
		$menu_mylinks='<a class="mmenu" href="my_links.php">'.$menu_item_links[$lang].' ('.$my_links_count.')</a>';
		$menu_favorites='<a class="mmenu" href="favorites.php">'.$menu_item_fav[$lang].'('.$favorites_count.')</a>';
		$menu_contacts='<a class="mmenu" href="contacts.php">'.$menu_item_contacts[$lang].'</a>';
		$menu_logger='<a class="mmenu" href="logger.php">'.$menu_item_logs[$lang].'</a>';
		$menu_trash='<b>'.$menu_item_trash[$lang].'('.$tr_n.')</b>';
		if (TOKEN==ADMIN_NAME) { $menu_stats=' | <a class="mmenu" href="stats.php"> Stats</a>'; }

		
	}
	elseif(preg_match("/calendar_view.php/i", $location))
	{
		$menu_main='<b>'.$menu_item_browser[$lang].'</b>';
		$menu_map='<a class="mmenu" href="chat_map.php">'.$menu_item_map[$lang].'</a>';
		$menu_search='<a class="mmenu" href="search_v2.php">'.$menu_item_search[$lang].'</a>';
		$menu_mylinks='<a class="mmenu" href="my_links.php">'.$menu_item_links[$lang].' ('.$my_links_count.')</a>';
		$menu_favorites='<a class="mmenu" href="favorites.php">'.$menu_item_fav[$lang].'('.$favorites_count.')</a>';
		$menu_contacts='<a class="mmenu" href="contacts.php">'.$menu_item_contacts[$lang].'</a>';
		$menu_logger='<a class="mmenu" href="logger.php">'.$menu_item_logs[$lang].'</a>';
		$menu_trash='<a class="mmenu" href="trash.php">'.$menu_item_trash[$lang].'('.$tr_n.')</a>';
		if (TOKEN==ADMIN_NAME) { $menu_stats=' | <a class="mmenu" href="stats.php"> Stats</a>'; }

		
	}
	elseif(preg_match("/chat_map.php/i", $location))
	{
		$menu_main='<a class="mmenu" href="'.$view_type.'">'.$menu_item_browser[$lang].'</a>';
		$menu_map='<b>'.$menu_item_map[$lang].'</b>';
		$menu_search='<a class="mmenu" href="search_v2.php">'.$menu_item_search[$lang].'</a>';
		$menu_mylinks='<a class="mmenu" href="my_links.php">'.$menu_item_links[$lang].' ('.$my_links_count.')</a>';
		$menu_favorites='<a class="mmenu" href="favorites.php">'.$menu_item_fav[$lang].'('.$favorites_count.')</a>';
		$menu_contacts='<a class="mmenu" href="contacts.php">'.$menu_item_contacts[$lang].'</a>';
		$menu_logger='<a class="mmenu" href="logger.php">'.$menu_item_logs[$lang].'</a>';
		$menu_trash='<a class="mmenu" href="trash.php">'.$menu_item_trash[$lang].'('.$tr_n.')</a>';
		if (TOKEN==ADMIN_NAME) { $menu_stats=' | <a class="mmenu" href="stats.php"> Stats</a>'; }

		
	}
	elseif(preg_match("/settings.php/i", $location))
	{
		$menu_main='<a class="mmenu" href="'.$view_type.'">'.$menu_item_browser[$lang].'</a>';
		$menu_map='<a class="mmenu" href="chat_map.php">'.$menu_item_map[$lang].'</a>';
		$menu_search='<a class="mmenu" href="search_v2.php">'.$menu_item_search[$lang].'</a>';
		$menu_mylinks='<a class="mmenu" href="my_links.php">'.$menu_item_links[$lang].' ('.$my_links_count.')</a>';
		$menu_favorites='<a class="mmenu" href="favorites.php">'.$menu_item_fav[$lang].'('.$favorites_count.')</a>';
		$menu_contacts='<a class="mmenu" href="contacts.php">'.$menu_item_contacts[$lang].'</a>';
		$menu_logger='<a class="mmenu" href="logger.php">'.$menu_item_logs[$lang].'</a>';
		$menu_trash='<a class="mmenu" href="trash.php">'.$menu_item_trash[$lang].'('.$tr_n.')</a>';
		if (TOKEN==ADMIN_NAME) { $menu_stats=' | <a class="mmenu" href="stats.php"> Stats</a>'; }
	}
	elseif(preg_match("/favorites.php/i", $location))
	{
		$menu_main='<a class="mmenu" href="'.$view_type.'">'.$menu_item_browser[$lang].'</a>';
		$menu_map='<a class="mmenu" href="chat_map.php">'.$menu_item_map[$lang].'</a>';
		$menu_search='<a class="mmenu" href="search_v2.php">'.$menu_item_search[$lang].'</a>';
		$menu_mylinks='<a class="mmenu" href="my_links.php">'.$menu_item_links[$lang].' ('.$my_links_count.')</a>';
		$menu_favorites='<b>'.$menu_item_fav[$lang].'('.$favorites_count.')</b>';
		$menu_contacts='<a class="mmenu" href="contacts.php">'.$menu_item_contacts[$lang].'</a>';
		$menu_logger='<a class="mmenu" href="logger.php">'.$menu_item_logs[$lang].'</a>';
		$menu_trash='<a class="mmenu" href="trash.php">'.$menu_item_trash[$lang].'('.$tr_n.')</a>';
		if (TOKEN==ADMIN_NAME) { $menu_stats=' | <a class="mmenu" href="stats.php"> Stats</a>'; }
	}

// check if archivization is currently enabled...
if ($sess->get('log_status') === false) { 

		$html->system_message($status_msg1[$lang]);
	
}

if ($start) { 

	$cur_loc="&start=$start"; 

}

// Show special contact?
$db->get_jorge_pref("3");
$show_spec = $db->result->pref_value;

// Display special contacts?
if ($show_spec === "2") {

		$db->spec_ignore(true);

        }
        else {

		$db->spec_ignore(false);

}

//Generate quick jump
$db->get_last_day();
$ql_date = $db->result->at;
$quick_link = $enc->crypt_url("tslice=$ql_date");

$html->menu('
	<a name="top"></a>
	<table border="0" cellspacing="0" class="ff" width="100%">
	<tr>
		<td colspan="2" height="29" style="text-align: right;">
		<a href="'.$view_type.'?a='.$quick_link.'" title="'.$qlink_l[$lang].'"><span style="font-weight: bold; color: white;">&#8656;</span></a>
		<b>'.TOKEN.'@'.XMPP_HOST.'</b>&nbsp; | &nbsp;
		<a href="settings.php">'.$menu_item_panel[$lang].'</a>&nbsp; | &nbsp;
		<a href="#" onClick="smackzk();">'.$sel_client[$lang].'</a>&nbsp; | &nbsp;
		<a href="help.php" target="_blank">'.$help_but[$lang].'</a>&nbsp; | &nbsp;<a href="index.php?act=logout">'.$log_out_b[$lang].'</a><hr size="1" noshade="noshade" style="color: #c9d7f1;"></td>
	</tr>
	<tr><td height="57"><a href="'.$view_type.'"><img src="img/'.$brand_logo.'" alt="logo" border="0"></a></td></tr>
	<tr><td valign="top" height="35"><form action="search_v2.php" method="post">
	<input id="t_search" type="text" name="query" class="cc" value="'.htmlspecialchars(stripslashes($search_phase)).'">
	');

if ($search_loc==1) {

	if (isset($_GET[c])) {
		
		$enc->decrypt_url($_GET['c']);
		$time2_start = $enc->time_start;
		$time2_end = $enc->time_end;
	
	}

	else{

		$time2_start=$_POST[time2_start];
		$time2_end=$_POST[time2_end];
		if (validate_date($time2_start) === false) { 
		
			unset($time2_start); 
		}
		if (validate_date($time2_start) === false) { 
		
			unset($time2_end); 
		}

	}
		
	if ($time2_start AND $time2_end) { 
	
			if (strtotime("$time2_start") > strtotime("$time2_end")) { 
			
					$alert = $time_range_w[$lang]; unset ($search_phase); 
					
			} 
	}

	$db->get_uniq_chat_dates();
	$result = $db->result;
	foreach ($result as $row) {

		$r++;
		$to_tble[$r] = $row[at];

	}

	$html->menu('<select name="time2_start" style="text-align: center;"><option value="">'.$time_range_from[$lang].'</option>');

	for ($t=1;$t<=$r;$t++) {

		$html->menu('<option value="'.$to_tble[$t].'"');

		if ($time2_start==$to_tble[$t]) {

			$html->menu('selected="selected"');
			
		}

		$html->menu('>'.$to_tble[$t].'</option>');
	
	}

	$pass_t=$t;
	
	$html->menu('</select>&nbsp;<select name="time2_end" style="text-align: center;"><option value="">'.$time_range_to[$lang].'</option>');

	for ($t=$r;$t>=1;$t--) {

		$html->menu('<option value="'.$to_tble[$t].'"');

		if ($time2_end==$to_tble[$t]) {

			$html->menu('selected="selected"');
		
		}
		
		$html->menu('>'.$to_tble[$t].'</option>');
	
	}

	$html->menu('</select>');

	if ($time2_start AND !$time2_end) { 
		
		$time2_end = $to_tble[$pass_t-1]; 
	
	}
	
	if (!$time2_start AND $time2_end) { 
	
		$time2_start = $to_tble[($t+1)-$t]; 
		
	}

}

$html->menu('<input type="submit" value="'.$search_box[$lang].'"></form></td>');

if ($mac_user === true) {

	$html->menu('<td align="right"><a href="#"><img border="0" alt="Hello Mac user!" width="16" height="16" src="img/apple-logo.png" /></a></td>');

}

$html->menu('
		</tr>
		<tr style="background-image: url(img/bell-bak.png); height: 24;">
			<td colspan="11" width="100%" style="text-align: left; padding-left: 30px; color: white;">
				'.$menu_main.' | '
				.$menu_map.' | '
				.$menu_favorites.' | '
				.$menu_mylinks.' | '
				.$menu_search.' | '
				.$menu_contacts.' | '
				.$menu_logger.$menu_stats.' | ' 
				.$menu_trash. 
				' | <a class="mmenu" href="#" onClick="window.location.reload()">'.$refresh[$lang].'</a>
			</td>
		</tr>
		</table>
		<p align="center"><b>'.$alert.'</b></p>
	');

// Get user roster.
$rpc_roster = $ejabberd_rpc->get_roster();

// creater roster object and rewrite it to portable multidimentional array
$ejabberd_roster = new roster();

foreach ($rpc_roster as $roster_record) {

	if ($roster_record[group]=="") { 

		$roster_record[group] = $con_no_g[$lang]; 
	
	}

	// avoid contacts without nick
	if ($roster_record[nick]!="") {

		$ejabberd_roster->add_item($roster_record[jid],$roster_record[nick],$roster_record[group]);
	
	}
}

?>
