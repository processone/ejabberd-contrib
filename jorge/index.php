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

// all we need is header.php file - be sure to include it in all Jorge files! as it containg authentication futures.
require_once("headers.php");
require_once("lib/recaptchalib.php");

// if already logged in (session active), move to main screen according to user preferences
if ($sess->get('uid_l')) { 

	if ($sess->get('view_type') == "1") { 

			header ("Location: main.php");

		}
		else {

			header ("Location: calendar_view.php");
	
	}

}

// get post data
$inpLogin = strtolower($_POST['inpLogin']);
$inpPass = $_POST['inpPass'];

if ($wo_sess || $inpLogin || $inpPass) {

	// set attepts in cookies
	if ($_COOKIE["auth_attempt"]=="") {

			// show captcha anyway if user comes for the first time or have disabled cookies
			setcookie("auth_attempt",1,time()+600);
			$auth_attempt = "3";

		}
		else{

			$auth_attempt = $_COOKIE["auth_attempt"] + 1;
			settype($auth_attempt,"string");
			// if it is not numeric let set it to some high value
			if (!ctype_digit($auth_attempt)) {

				$auth_attempt = "100";

			}

			setcookie("auth_attempt",$auth_attempt,time()+600);
	}

	// on 3rd attempt - check captcha
	if ($auth_attempt >= "3") {

        		$resp = recaptcha_check_answer(CAPTCHA_PRIVATE,
                		                  $_SERVER["REMOTE_ADDR"],
                       		 	          $_POST["recaptcha_challenge_field"],
						  $_POST["recaptcha_response_field"]);


			if (!$resp->is_valid) { 

					unset($inpPass);
					unset($inpLogin);
					$html->system_message($wrong_data2[$lang]);
		
			}

	}

}

if ($_GET['act']==="logout") {

		if ($db->get_user_id(TOKEN) === true) {
		
			if($db->result->user_id) {
			
				$db->set_user_id($db->result->user_id);
				$db->set_logger("2","1",$rem_adre);
			
			}
		
		}

		$sess->finish();
		header("Location: index.php");
		exit;
	
	} 
	
	else {

		if ($inpLogin!="" || $inpPass!="") {

			$ejabberd_rpc->set_user($inpLogin,$inpPass);
			if ($ejabberd_rpc->auth() === true) {

	          		$sess->set('login',$inpLogin);
		  		$sess->set('uid_l',$inpLogin);
		  		$sess->set('uid_p',$enc->crypt_url("single=$inpPass"));
				$sess->set('vhost',XMPP_HOST);
				// remember user choice
				setcookie("fav_host", XMPP_HOST,time()+2592000);
				setcookie("auth_attempt",0,time()+2592000);
				// Get user_id if it is possible
				if ($db->get_user_id($sess->get('uid_l')) === true) {

						$ui = $db->result->user_id;
						$db->set_user_id($ui);
						$db->is_log_enabled();
						$ret_v = $db->result->is_enabled;

					}
					else {
						
						$ret_val = null;
				}

		  		if ($ret_v === true OR $ret_v === false) {

					$sess->set('log_status',$ret_v);
					$db->set_logger("1","1",$rem_adre);
					// get preferences, if not set, fallback to standard view.
					$db->get_jorge_pref();
					$pref_res = $db->result;
					foreach ($pref_res as $res_pref) {

							if ($res_pref[pref_id]=="1") {

									if ($res_pref[pref_value] == "2") {

											$view_type = "2"; 
											$tmp_v = "calendar_view.php"; 
									
									}
										elseif($res_pref[pref_value] == "1") {

											$view_type = "1"; 
											$tmp_v = "main.php"; 
								
									}
								
								$sess->set('view_type',$view_type);
							}
			
						if ($res_pref[pref_id] == "2") {

								// Check if language is supported, return value if it is
								$check_language = is_language_supported($res_pref[pref_value],$language_support,1,true);

								// function can return true/false/value, in this case we need value and not false
								if ($check_language !== false) {

										// set language according to database setup
										setcookie("jorge_language",$check_language,time()+2592000);
										$sess->set('language',$check_language);

									}
									else{

										// this is where language was not found in settings, so use default
										setcookie("jorge_language",$language_support[default_language][1],time()+2592000);
										$sess->set('language',$check_language);

								}

						}
					}

					if ($tmp_v=="") { 

							$sess->set('view_type',2); 
							$tmp_v="calendar_view.php"; 
						}

					header("Location: $tmp_v");
					exit; // lets break script at this point...
				
				}
		  		
				else {

					$sess->set('log_status',null);
					header("Location: not_enabled.php"); 
					exit;
				}

			}

		if (no_vhost === true) {

				$html->system_message($vhost_not_selected[$lang]);
			
			}
			else{

				$html->system_message($wrong_data[$lang]);
		
		}
		$db->get_user_id($inpLogin);
		$ui_fail = $db->result->user_id;

		// Workaround, if user_id is not know, do not alter login attempts
		if ($ui_fail) {

				$db->get_last_attempt($ui_fail);
				$row = $db->result->cnt;

			}
			else{

				$row="0";

		}

		// bump log_level if more then 3 log attempts in one minute
		if ($row > "3") { 

				$log_level = "3"; 
		
		} 
		else { 

			$log_level = "2";
		
		} 

		if ($ejabberd_rpc->check_account() === true) {

			$db->set_user_id($ui_fail);
			$db->set_logger("3",$log_level,$rem_adre);
		
		}

	}
 
}


$html->set_body('

        	<script type="text/javascript">
         		var RecaptchaOptions = {
             			theme : \'clean\'
              		};
        	</script>

	');

$html->set_body('
		<br><div align="center" style="height: 110;"><br><a href="index.php"><img border="0" alt="Branding logo" src="img/'.$brand_logo.'"></a></div>
		<table class="ff" cellspacing="0" width="100%">
		<tr style="background-image: url(img/bell-bak.png); height: 24;">
		<td style="text-align: left; padding-left: 10px; color: white;">'.$welcome_1[$lang].'</td>
		</tr>
		<tr>
			<td style="text-align: right;">
				<form name="language_selector" action="index.php" method="get">
					<select class="cc" name="lng_sw" onchange="javascript:document.language_selector.submit();">
');

// Get supported languages.
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

$html->set_body('</select>
		</form>
		</td>
		</tr>
		</table>
		<center>
		<form action="index.php" method="post">
		<br><br>
		<table class="ff" border="0" cellspacing="0" cellpadding="0">
		<tr><td align="right">'.$login_w[$lang].'&nbsp;</td><td><input name="inpLogin" value="'.$_POST['inpLogin'].'" class="log" >
		
		');

// vhost support
$vhost_count = count($vhosts);
if ($vhost_count>1) {

		$html->set_body('<select name="vhost">
				<option value="null">'.$vhost_select[$lang].'</option>
		');

		while (array_keys($vhosts)) {

			$vhost = key($vhosts);
			if ($_POST['vhost'] == $vhost OR $_COOKIE['fav_host'] == $vhost) {

					$selected_vhost="selected=\"selected\"";

				}
				else {

					unset($selected_vhost);

			}

			$html->set_body('<option value="'.$vhost.'" '.$selected_vhost.'>'.$vhost.'</option>');
			array_shift($vhosts);

		}

		$html->set_body('</select>');


	}
	else{

		// There is only one vhost configured, so do not display select box
		$html->set_body('@'.key($vhosts).'<input type="hidden" name="vhost" value="'.key($vhosts).'">');

}
		
$html->set_body('</td></tr>
		<tr style="height: 3pt;" ><td></td></tr>
		<tr><td align="right">'.$passwd_w[$lang].'&nbsp;</td><td><input name="inpPass" type="password" class="log"></td></tr>
		<tr style="height: 10pt;"><td></td></tr>
		');

		// display captcha on 3rd attempt...
		$check_cookie = $_COOKIE["auth_attempt"];
		settype($check_cookie,"string");
		if (!ctype_digit($check_cookie) OR $check_cookie=="") {

				$cookie_failed = true;
			
			}
			else{

				$cookie_failed = false;

		}

		if ($check_cookie >= "2" OR $cookie_failed === true) {

			$html->set_body('<tr><td colspan="2">'.recaptcha_get_html(CAPTCHA_PUBLIC,$error = null, $use_ssl = true).'</td></tr>
					<tr style="height: 15;"><td></td></tr>');

		}

		$html->set_body('<tr><td colspan="2" align="right"><input type="submit" name="sublogin" value="'.$login_act[$lang].'"></td></tr>
		</table></form></center>	
		');

require_once("footer.php");
?>
