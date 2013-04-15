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
$init = $_POST[init];

// header
$html->set_overview('<h2>'.$fav_main[$lang].'</h2><small>'.$fav_desc[$lang].'</small>');

// add to favorites
if ($init == "1") {

	if ($enc->decrypt_url($_POST[a]) === true) {

		$tslice = $enc->tslice;
		$peer_name_id = $enc->peer_name_id;
		$peer_server_id = $enc->peer_server_id;
		$db->get_user_name($peer_name_id);
		$user_name = $db->result->username;
		$db->get_server_name($peer_server_id);
		$server_name = $db->result->server_name;
		$nick_name =  query_nick_name($ejabberd_roster,$user_name, $server_name);
		if (!$nick_name) {

				$nick_name = $not_in_r[$lang];
				unset($malpa);
			
			}
			else{

				$malpa = "@";

		}
		$html->set_body('
				<center>'.$fav_add[$lang].':<br>
				<table class="ff" border="0" cellspacing="0">
				<tr class="main_row_b"><td style="text-align: center;">'.$fav_chat[$lang].'<b>'.cut_nick($nick_name).'</b> (<i>'.$user_name.$malpa.$server_name.'</i>)</td></tr>
				<form style="margin-bottom: 0;" action="favorites.php" method="post">
				<tr><td colspan="3" align="center">
					<textarea class="ccc" name="desc" rows="4">'.$my_links_optional[$lang].'</textarea>
				</td></tr>
				<tr><td colspan="3" align="center">
						<input type="hidden" name="favorite" value="'.$_POST[a].'">
						<input name="trigger" class="red" type="submit" value="'.$my_links_commit[$lang].'">
						<input class="red" type="button" value="'.$my_links_cancel[$lang].'" onClick="parent.location=\''.$view_type.'?a='.$_POST[a].'\'">
					</td></tr>
				</form></table>
				<hr size="1" noshade="noshade">
				</center>');
	
	}

}

// process request
if ($_POST[favorite]) {

	if ($enc->decrypt_url($_POST[favorite]) === true) {


		if ($db->set_favorites($enc->peer_name_id,$enc->peer_server_id,$peer_resource_id,$enc->tslice,$_POST[desc]) === true) {

				$html->status_message($fav_success[$lang]);

			}
			else{

				$html->alert_message($fav_error[$lang]);
			
			}

	}

}

// delete favorites
if ($_GET[del] == "t") {

	if ($db->delete_favorites_id($_GET[link_id]) === true) {


			$html->status_message($fav_removed[$lang]);

		}
		else{

			$html->alert_message($oper_fail[$lang]);
			
	}

}

// get favorites
$db->get_favorites();

if (count($db->result)>0) {

		$html->set_body('
                                <center><table id="maincontent" class="ff" cellspacing="0">
                                <tr class="header"><td>'.$fav_when[$lang].'</td><td>'.$fav_contact[$lang].'</td><td>'.$fav_comment[$lang].'</td></tr>
                                <tr class="spacer" height="1px"><td colspan="4"></td></tr>
                                <tbody id="searchfield">	
		');
		$fav_list = $db->result;
		foreach ($fav_list as $row) {

			$db->get_user_name($row[peer_name_id]);
			$user_name = $db->result->username;
			$db->get_server_name($row[peer_server_id]);
			$server_name = $db->result->server_name;
			$nickname = query_nick_name($ejabberd_roster,$user_name,$server_name);
			$to_base = $enc->crypt_url("tslice=$row[tslice]&peer_name_id=$row[peer_name_id]&peer_server_id=$row[peer_server_id]");
			if (!$row[comment] OR $row[comment] == $my_links_optional[$lang]) {

					$comment = $fav_nocomm[$lang];
					
				}
				else{

					$comment = htmlspecialchars($row[comment]);
					$comment = str_replace("\n","<br>",$comment);
					$comment = wordwrap($comment,30,"<br>",true);
			
			}
			if (!$nickname) { 
			
					$nickname = $not_in_r[$lang]; 
					unset($malpa);
			
				}
				else {

					$malpa = "@";

			}
			$html->set_body('
				<tr style="cursor: pointer;" bgcolor="#e8eef7" onMouseOver="this.bgColor=\'c3d9ff\';" onMouseOut="this.bgColor=\'#e8eef7\';">
				<td onclick="window.location=\''.$view_type.'?a='.$to_base.'&loc=3\';" style="padding-left: 10px; padding-right: 10px">'.verbose_date($row[tslice],$months_names,$weekdays).'</td>
				<td onclick="window.location=\''.$view_type.'?a='.$to_base.'&loc=3\';" style="padding-left: 10px; padding-right: 10px">
					<b>'.htmlspecialchars(cut_nick($nickname)).'</b> (<i>'.htmlspecialchars($user_name).$malpa.htmlspecialchars($server_name).'</i>)
				</td>
				<td onclick="window.location=\''.$view_type.'?a='.$to_base.'&loc=3\';" style="padding-left: 10px; padding-right: 10px;">'.$comment.'</td>
				<td style="text-align: center;"><a href="favorites.php?del=t&link_id='.$row[link_id].'" onClick="if (!confirm(\''.$del_conf_my_link[$lang].'\')) return false;">&nbsp;'.$fav_remove[$lang].'&nbsp;</td>
				</tr>
			');

		}
		$html->set_body('</tbody><tr class="spacer"><td colspan="4"></td></tr><tr class="foot"><td colspan="4" height="15"></td></tr></table></center>');

	}

	else {

		$html->status_message('<center><div class="message" style="width: 450px;">'.$fav_empty[$lang].'</div></center>');
	
}

require_once("footer.php");
?>
