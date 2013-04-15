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

$tigger = $_POST['trigger'];
$desc = $_POST['desc'];
$del = $_GET['del'];
$link_id = $_GET['link_id'];

if ($_GET[a]) {

		if ($enc->decrypt_url($_GET[a]) === true) {

				$variables[tslice] = $enc->tslice;
				$variables[peer_name_id] = $enc->peer_name_id;
				$variables[peer_server_id] = $enc->peer_server_id;
				$variables[lnk] = $enc->lnk;
				$variables[linktag] = $enc->linktag;
				$variables[strt] = $enc->strt;
				$variables[ismylink] = $enc->ismylink;

			}
			else {
				unset($variables);

			}

}

if ($del === "t") {

		if ($db->del_mylink($link_id) === true) {
		
				$html->status_message($my_links_removed[$lang]);
				
				// recount number of links
				$db->get_mylinks_count();
				$my_links_count = $db->result->cnt;

			}

			else {
				
				$html->alert_message($oper_fail[$lang]);
			
			}

}


if ($tigger === $my_links_commit[$lang]) {

		if ($enc->decrypt_url($_POST[hidden_field]) === true) {
			
				$peer_name_id = $enc->peer_name_id;
				$peer_server_id = $enc->peer_server_id;
				$datat = $enc->tslice;
				$lnk = $enc->lnk;
				$strt = $enc->strt;
				$linktag = $enc->linktag;
				$link = $lnk."&start=$strt#$linktag";
				if ($desc === $my_links_optional[$lang]) { 
	
						$desc = $my_links_none[$lang]; 
			
					}
		
				$desc = substr($desc,0,120);
				if($db->add_mylink($peer_name_id,$peer_server_id,$datat,$link,$desc) === true) {

						$html->status_message($my_links_added[$lang].'
									<br><a href="'.$view_type.'?a='.$link.'" style="color: blue;"><b>'.$my_links_back[$lang].'</b></a></div></center>');
						
						// recount number of links
						$db->get_mylinks_count();
						$my_links_count = $db->result->cnt;

					}
					else{

						$html->alert_message($oper_fail[$lang]);

				}
	
		}

}

if ($variables[ismylink] === "1") {

	$db->get_server_name($enc->peer_server_id);
	$sname = $db->result->server_name;
	$db->get_user_name($enc->peer_name_id);
	$uname = $db->result->username;
	$nickname=query_nick_name($ejabberd_roster,$uname,$sname);
	$jid=''.$uname.'@'.$sname.'';
	$hidden_fields = $enc->crypt_url("tslice=$enc->tslice&peer_name_id=$variables[peer_name_id]&peer_server_id=$variables[peer_server_id]&lnk=$variables[lnk]&strt=$variables[strt]&linktag=$variables[linktag]");
	if (!$nickname) {

			$nickname = $not_in_r[$lang];
			$jid = $sname;

	}
	$html->set_body('
		<center>
		'.$my_links_save_d[$lang].'<br />
		<table class="ff" border="0" cellspacing="0">
		<form action="my_links.php" method="post">
		<tr><td height="5"></td></tr>
		<tr class="main_row_b"><td style="text-align:center;">'.$my_links_chat[$lang].'&nbsp;&nbsp;
		<b>'.cut_nick($nickname).'</b> (<i>'.htmlspecialchars($jid).'</i>)</td></tr>
		<tr><td height="5"></td></tr>
		<tr><td colspan="3" align="center"><textarea class="ccc" name="desc" rows="4">'.$my_links_optional[$lang].'</textarea></td></tr>
		<tr><td colspan="3" align="center"><input name="trigger" class="red" type="submit" value="'.$my_links_commit[$lang].'">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
		<input class="red" type="button" value="'.$my_links_cancel[$lang].'" onClick="parent.location=\''.$view_type.'?a='.$enc->lnk.'&start='.htmlspecialchars($enc->strt).'#'.htmlspecialchars($enc->linktag).'\'"></td>
		</tr><tr><td><input type="hidden" name="hidden_field" value="'.$hidden_fields.'"></form></table></center>
		<br><br><br><br>
		');

}

$html->set_overview('<h2>'.$my_links_desc_m[$lang].'</h2><small>'.$my_links_desc_e[$lang].'</small>');

if ($my_links_count === "0") { 
		
		$html->status_message($my_links_no_links[$lang]);
		
	}

	else {

		$html->set_body('
				<center><table id="maincontent" class="ff" cellspacing="0">
				<tr class="header"><td>'.$my_links_link[$lang].'</td><td>'.$my_links_chat[$lang].'</td><td>'.$my_links_desc[$lang].'</td></tr>
				<tr class="spacer" height="1px"><td colspan="4"></td></tr>
				<tbody id="searchfield">
			');
		$db->get_mylink();
		$result = $db->result;
		foreach ($result as $entry) {

			$db->get_user_name($entry[peer_name_id]);
			$peer_name = $db->result->username;
			$db->get_server_name($entry[peer_server_id]);
			$peer_server = $db->result->server_name;
			$nickname=query_nick_name($ejabberd_roster,$peer_name,$peer_server);
			$desc = htmlspecialchars($entry[description]);
			$jid = $peer_name.'@'.$peer_server;
			if (!$nickname) {

				$nickname = $not_in_r[$lang];
				$jid = $peer_server;
			
			}
			$html->set_body('
					<tr style="cursor: pointer;" bgcolor="#e8eef7" onMouseOver="this.bgColor=\'c3d9ff\';" onMouseOut="this.bgColor=\'#e8eef7\';">
					<td onclick="window.location=\''.$view_type.'?loc=4&a='.$entry['link'].'\';" style="padding-left: 10px; padding-right: 10px">'.verbose_date($entry['datat'],$months_names).'</td>
					<td onclick="window.location=\''.$view_type.'?loc=4&a='.$entry['link'].'\';">&nbsp;<b>'.cut_nick(htmlspecialchars($nickname)).'</b> ('.htmlspecialchars($jid).')&nbsp;</td>
					<td onclick="window.location=\''.$view_type.'?loc=4&a='.$entry['link'].'\';">&nbsp;'.$desc.'</td>
					<td><a href="my_links.php?del=t&link_id='.$entry[id_link].'" onClick="if (!confirm(\''.$del_conf_my_link[$lang].'\')) return false;" >&nbsp;'.$del_my_link[$lang].'&nbsp;</a></td>
					</tr>
				');
		}

		$html->set_body('
				</tbody><tr class="spacer"><td colspan="4"></td></tr><tr class="foot"><td colspan="4" height="15"></td></tr></table></center>
			');

}
require_once("footer.php");

?>
