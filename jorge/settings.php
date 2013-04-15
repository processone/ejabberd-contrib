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

$tgle = $_POST['toggle'];
$vspec = $_POST['v_spec'];

// Check post data. Compare them to session info.
if ($_POST['del_all'] OR $_POST['erase_confirm']) {

	debug(DEBUG,"Processing archive removal:");
	if ($sess->get('validate_number') !== $_POST['validate_form']) {

			debug(DEBUG," - Invalid control number, destroying POST data. Control should be: ".$sess->get('validate_number'));
			unset($_POST);
			$html->alert_message($oper_fail[$lang]);
		
		}
		else{

			debug(DEBUG," - POST data seems to be ok.");

	}

}

// Generate new control data for forms
$set_control = md5(rand(10000,10000000));
$sess->set("validate_number",$set_control);
debug(DEBUG,"Setting new control data: $set_control");

// toggle message saving
if ($tgle) { 

	debug(DEBUG,"Trying to change archiving option");
	if ($tgle === $arch_on[$lang]) {
				
				if($db->set_log(true) === true) {

						$sess->set('log_status',true);
						$db->set_logger("7","1");
						$html->status_message($status_msg2[$lang]);
					
					}
					else{
						
						$html->alert_message($oper_fail[$lang]);
					
					}

		}
		elseif($tgle === $arch_off[$lang]) {
				
				if($db->set_log(false) === true) {

						$sess->set('log_status',false);
						$db->set_logger("6","1");
						$html->status_message($status_msg3[$lang]);
						$html->system_message($status_msg1[$lang]);
					
					}
					else{

						$html->alert_message($oper_fail[$lang]);

					}

		}
}

// Control diplaying of special contacs
if ($vspec) {

	if ($db->set_jorge_pref("3",$vspec) === true) {

			$html->status_message($con_saved[$lang]);

		}
		else{

			$html->alert_message($oper_fail[$lang]);

	}

}

// delete entire archive
if ($_POST['erase_confirm'] === "true") {

	if ($_POST['del_all'] === $settings_del[$lang]) {

		debug(DEBUG," - Trying to erase all message archives");
		if ($db->erase_all() === true) {

				$html->status_message($deleted_all[$lang]);
				$db->set_logger("9","2");
				debug(DEBUG," - DONE");

			}
			else{
	
				$html->alert_message($delete_error[$lang]);
				debug(DEBUG," - FAILED");

		}

	}

}

// set own name
if ($_POST['own_name_value']) {

		// Validation is done by class, so we pass values there...
		if ($db->set_own_name($_POST['own_name_value']) === true) {

				$html->status_message($con_saved[$lang]);

			}else{

				$html->alert_message($oper_fail[$lang]);

		}

}

$html->set_overview('<h2>'.$settings_desc[$lang].'</h2><small>'.$settings_desc_detail[$lang].'</small>');
$html->set_body('<center><table><form action="settings.php" method="post">
		<tr style="font-size: x-small;"><td>'.$setting_d1[$lang].'</td><td><input class="settings" type="submit" name="toggle" value="');
$db->is_log_enabled();
if ($db->result->is_enabled === false) { 
		
		$html->set_body($arch_on[$lang]);
		
	} 
	else { 
		
		$html->set_body($arch_off[$lang]);
		
	}

$html->set_body('"></td></tr></form>');

$html->set_body('<form action="settings.php" method="post">
	<tr style="font-size: x-small;"><td>'.$setting_d2[$lang].'</td>
	<td>
		<input class="settings" type="submit" name="del_all" value="'.$settings_del[$lang].'" onClick="if (!confirm(\''.$del_all_conf[$lang].'\')) return false;">
		<input type="hidden" name="erase_confirm" value="true">
		<input type="hidden" name="validate_form" value="'.$set_control.'">
	</td>
	</tr>
	</form>
	');

if ($db->get_jorge_pref("3") === false) {

	$html->alert_message($oper_fail[$lang]);

}

$special_select = $db->result->pref_value;

if ($special_select === "2") {

		$n_is_sel = "selected";

	}
	else{

		$y_is_sel = "selected";

}

$html->set_body('

	<form action="settings.php" method="post" name="tggle_special">
	<tr style="font-size: x-small;"><td>'.$spec_contact_enable[$lang].'(<a href="#" title="'.$spec_contact_desc[$lang].'">?</a>)</td>
	<td><select class="settings" name="v_spec" size="0" onchange="javascript:document.tggle_special.submit();">
	<option '.$y_is_sel.' value="1">'.$sel_yes[$lang].'</option>
	<option '.$n_is_sel.' value="2">'.$sel_no[$lang].'</optin>
	</select>
	</td></tr></form>

');

$html->set_body('<form action="settings.php" method="get" name="save_pref">
	<tr style="font-size: x-small;"><td>'.$select_view[$lang].'</td>
	<td><select class="settings" name="v" onchange="javascript:document.save_pref.submit();">
');

if ($sess->get('view_type') == "1") { 

		$std="selected"; 
		
	} 
	else { 
	
		$cal="selected"; 
		
}
$html->set_body('
	<option '.$std.' value="1">'.$view_standard[$lang].'</option>
	<option '.$cal.' value="2">'.$view_calendar[$lang].'</optin>
	</select>
	<input name="set_pref" type="hidden" value="1">
	<input name="ref" type="hidden" value="settings">
	</td></tr>
	</form>
');

$html->set_body('<form action="settings.php" method="get" name="save_pref_lang">
	<tr style="font-size: x-small;"><td>'.$sel_language[$lang].'</td>
	<td><select class="settings" name="lng_sw" onchange="javascript:document.save_pref_lang.submit();">
');

while (array_keys($language_support)) {

	$lang_key = key($language_support);
	if ($sess->get('language') === $language_support[$lang_key][0]) {

			$pol_sel="selected";
		
		}
		else{

			unset($pol_sel);

	}
	$html->set_body('<option '.$pol_sel.' value="'.$language_support[$lang_key][1].'">'.$lang_key.'</option>');
	array_shift($language_support);

}

$html->set_body('
	</select>
	<input name="set_pref" type="hidden" value="2">
	<input name="ref" type="hidden" value="settings">
	</td></tr></form>
');

$html->set_body('<form action="settings.php" method="post" name="set_own_name">
	<tr style="font-size: x-small;"><td>'.$own_name_enter[$lang].'(<a href="#" title="'.$own_name_desc[$lang].'">?</a>)</td>
	<td>
	<input name="own_name_value" class="settings" type="text" value="');

// Check if own_name is set.
$db->get_own_name();
$own_name = $db->result->own_name;
if (!$own_name) {

		$html->set_body(TOKEN);

	}
	else{

		$html->set_body(htmlspecialchars($own_name));

}

$html->set_body('" maxlength="60">
	<input class="settings" type="submit" value="'.$own_name_commit[$lang].'">
	</td>
	</tr></form>');

$html->set_body('</table><hr size="1" noshade="noshade" style="color: #c9d7f1;"><br><small><b>'.$stats_personal_d[$lang].'</b></small>');

$db->get_personal_sum();
$total_messages = number_format($db->result->cnt);

$html->set_body('<p style="font-size: x-small;">'.$stats_personal[$lang].'<b> '.$total_messages.'</b></p><small><b>'.$stats_personal_top[$lang].'</b></small><br><br>');

$db->get_personal_top();
$results = $db->result;

if (count($results)!=0) {

		$html->set_body('<table bgcolor="#ffffff" class="ff" cellspacing="0" cellpadding="3">
				<tr style="background-image: url(img/bar_new.png); background-repeat:repeat-x; color: #fff; font-weight: bold;">
				<td>'.$stats_personal_count[$lang].'</td><td style="text-align: center;">'.$stats_peer[$lang].'</td><td>'.$stats_when[$lang].'</td></tr>
			');

		foreach ($results as $result) {

			$db->get_user_name($result[peer_name_id]);
			$user_name = $db->result->username;
			$db->get_server_name($result[peer_server_id]);
			$server_name = $db->result->server_name;
			$nickname=query_nick_name($ejabberd_roster,$user_name,$server_name);
			$to_base = $enc->crypt_url("tslice=$result[at]&peer_name_id=$result[peer_name_id]&peer_server_id=$result[peer_server_id]");
			$html->set_body('
				<tr><td style="text-align: center; font-weight: bold;">'.$result[count].'</td><td><b>'.$nickname.'</b>&nbsp;
				<small>('.htmlspecialchars($user_name).'@'.htmlspecialchars($server_name).')</small>
				</td><td><a id="pretty" title="'.$stats_see[$lang].'" href="'.$view_type.'?a='.$to_base.'"><u>'.$result[at].'</u></a></td></tr>
			');

		}

		$html->set_body('<tr height="15" style="background-image: url(img/bar_new.png); background-repeat:repeat-x; color: #fff;"><td colspan="3"></td></tr></table>');

	}
	else {

		$html->set_body('<div class="message">'.$no_archives[$lang].'</div>');

}

$html->set_body('</center>');
require_once("footer.php");
?>
