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

$html->set_overview('<h2>'.$trash_name[$lang].'</h2><small>'.$trash_desc[$lang].'</small>');

if ($enc->decrypt_url($_GET[a]) === true) {

		$tslice = $enc->tslice;
		$talker = $enc->peer_name_id;
		$server = $enc->peer_server_id;
		$action = $enc->action;
		$lnk = $enc->lnk;

	}
	else {

		$action = null;
	
}

if ($_GET['idx']) {

	$idx = $_GET['idx'];

	if ($enc->decrypt_url($idx) === true) {

			if($db->set_ext_index($enc->single) !== true) {

				unset($idx);
				unset($action);

			}

			$idx = $enc->single;

		}
		else{

			unset($idx);
			unset($action);

	}

}

if ($action=="undelete") {

		if ($db->move_chat_from_trash($talker,$server,$tslice,$lnk,$idx) === true) {

				$back_link = $enc->crypt_url("tslice=$tslice&peer_name_id=$talker&peer_server_id=$server");
                		$html->status_message($undo_info[$lang].'<br><a href="'.$view_type.'?a='.$back_link.'" style="color: blue;">'.$trash_vit[$lang].'</a>');
				
				// number of items in trash
				$db->get_trash_count();
				$tr_n = $db->result->cnt;

        		}

        	else

        	{

                		unset($talker);
				$html->alert_message($oper_fail[$lang]);

        	}

}

if ($action==="delete") {

		if ($db->remove_messages_from_trash($talker,$server,$tslice) === true) {

			$html->status_message($del_info[$lang]);
			// number of items in trash
			$db->get_trash_count();
			$tr_n = $db->result->cnt;

		}
		else {

			$html->alert_message($oper_fail[$lang]);

		}


}

if ($tr_n < 1) {
	
		$html->set_body('<p align="center"><b>'.$trash_empty[$lang].'</b></p>' );
	}

	else

	{
		$html->set_body('
				<center>
				<table id="maincontent" class="ff" align="center" border="0"  cellspacing="0">
				<tr class="header"><td style="padding-right: 15px;">'.$my_links_chat[$lang].'</td><td style="padding-right: 15px;">'.$logger_from_day[$lang].'</td><td>'.$del_time[$lang].'</td></tr>
				<tr class="spacer"><td colspan="5"></td></tr><tbody id="searchfield">
			');
		$db->get_trashed_items();
		$result = $db->result;
		foreach ($result as $entry) {
			
			$db->get_user_name($entry[peer_name_id]);
			$talker = $db->result->username;
			$db->get_server_name($entry[peer_server_id]);
			$server_name = $db->result->server_name;
			$tslice = $entry["date"];
			$nickname = query_nick_name($ejabberd_roster,$talker,$server_name);
			if (!$nickname) {

					$nickname = $not_in_r[$lang];
					unset($malpa);

				}
				else {

					$malpa = "@";

			}
			$reconstruct_link = $enc->crypt_url("tslice=$tslice&peer_name_id=$entry[peer_name_id]&peer_server_id=$entry[peer_server_id]");
			$undelete_link = $enc->crypt_url("tslice=$tslice&peer_name_id=$entry[peer_name_id]&peer_server_id=$entry[peer_server_id]&lnk=$reconstruct_link&action=undelete");
			$delete_link = $enc->crypt_url("tslice=$tslice&peer_name_id=$entry[peer_name_id]&peer_server_id=$entry[peer_server_id]&lnk=$reconstruct_link&action=delete");
			$idx = $enc->crypt_url("single=".$entry[idx]."");

			$html->set_body('
					<tr><td style="padding-left: 10px; padding-right: 10px;"><b>'.$nickname.'</b> (<i>'.htmlspecialchars($talker).$malpa.htmlspecialchars($server_name).'</i>)</td>
					<td style="text-align: center;">'.$tslice.'</td>
					<td style="padding-left: 5px; padding-right: 5px; font-size: x-small;">'.$entry[timeframe].'</td>
					<td style="padding-left: 10px;"><a href="trash.php?a='.$undelete_link.'&amp;idx='.$idx.'">'.$trash_undel[$lang].'</a></td>
					<td style="padding-left: 10px;"><a href="trash.php?a='.$delete_link.'&amp;idx='.$idx.'" onClick="if (!confirm(\''.$del_conf[$lang].'\')) return false;">'.$trash_del[$lang].'</a></td></tr>
					
				');

		}
		
		$html->set_body('
				</tbody><tr class="spacer"><td colspan="5"></td></tr><tr class="foot"><td colspan="5" height="15"></td></tr></table></center>
			');
	}

require_once("footer.php");
?>
