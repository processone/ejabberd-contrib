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
require_once("headers.php");
require_once("upper.php");

$html->set_overview('<h2>'.$chat_map[$lang].'</h2>'.'<small>'.$chat_select[$lang].'</small>');

if ($_POST['chat_map']) {
		
		$con_map = $enc->decrypt_url($_POST['chat_map']);

	}
	elseif ($_GET['chat_map']) {

		$con_map = $enc->decrypt_url($_GET['chat_map']);
	
	}

if($con_map === true) {

		$con_map = $enc->jid;
	}
	else{

		unset($con_map);
}
	
// prepare roster object
$ejabberd_roster->sort_by_nick("az");
$roster_chat = $ejabberd_roster->get_roster();

$html->set_body('<br><br><br>
		<form id="c_map_form" action="chat_map.php" method="post" name="chat_map_form">
		<p>'.$filter_tip[$lang].'</p>
		<span style="padding-right: 20px">'.$chat_m_select[$lang].'</span>
		<select id="c_map" style="text-align: center; border: 0px; background-color: #6daae7; color:#fff; font-size: x-small;" name="chat_map" size="0" onchange="javascript:document.chat_map_form.submit();">
		<option value="null">'.$chat_c_list[$lang].'</option>
	');

	while (array_keys($roster_chat)) {
		
		$jid = key($roster_chat);
		$roster_item = array_shift($roster_chat);
		$name = $roster_item[nick];
		$grp  = $roster_item[group];
		if ($con_map==$jid) { 
		
				$selected="selected"; 
				
			} 
			else { 
			
				$selected=""; 
				
		}
		
		$html->set_body('<option '.$selected.' value=\''.$enc->crypt_url("jid=$jid").'\'>'.htmlspecialchars($name).' ('.htmlspecialchars($grp).')</option>');

	}

$html->set_body('</select></form>');

if ($con_map AND $_POST['chat_map'] != "null") {

	// split username and server name
	list($name_peer,$server_peer) = split("@",$con_map);

	// get the id's of user and server
	$db->get_user_id($name_peer);
	$peer_name_id = $db->result->user_id;
	$db->get_server_id($server_peer);
	$peer_server_id = $db->result->server_id;

	if ($peer_name_id !== null AND $peer_server_id !== null) {
	
		//first get the months
		$db->get_chat_map($peer_name_id,$peer_server_id);
		$result1 = $db->result;
		$cc_cmp = count($result1);
		foreach ($result1 as $row_m) {

			// hack for proper date parsing
			list($y,$m) = split("-",$row_m[at]);
			$mo="$y-$m";
		
			// now get the days in with user was talking
			$db->get_chat_map_specyfic($peer_name_id,$peer_server_id,$mo);
			$result2 = $db->result;
			
			foreach($result2 as $row_day) {

					// now scan day for chats, yep thats weak, but as long as we dont have right stats table this will work...
					$i++;
					list($y,$m,$d) = split("-",$row_day[at]);
					$days[$i] = $d;
			}

			if (count($days)>=1) {
				
					$html->set_body('<table cellpadding="0" cellspacing="0" style="display:inline;"><tr><td style="width:200px; border: 0px; text-align:center;">');
					$html->set_body(calendar($db,$user_id,$xmpp_host,$y,$m,$days,TOKEN,$url_key,$left,$right,$selected,$lang,$view_type,2,$peer_name_id,$peer_server_id,$cal_days,$enc,$months_names,$weekdays));
					$html->set_body('</td></tr></table>');
					unset($days);
				
				}
			else {
			
				$score++;

			}
		$i=0;

		}

	}

	else {

		$cc_cmp = $score;
	}


	if ($score==$cc_cmp) { 

		$html->set_body('<span style="text-align: center;"><h2>'.$chat_no_chats[$lang].'</h2></span>');
	
	}

}

$html->set_body('<br><small><i>*-'.$ff_notice[$lang].'</i></small>');
require_once("footer.php");
?>
