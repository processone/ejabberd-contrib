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

if (ADMIN_NAME !== TOKEN) { 

	print 'no access'; 
	exit; 

}

// vhost stats
$vhost_selected = $_POST['stats_vhost'];
if (array_key_exists($vhost_selected, $vhosts) === true) {

		$vhost_valid = true;
		$vhost_active = $vhost_selected;

	}
	else{

		$vhost_valid = false;
		$vhost_active = null;

}

$html->set_body('<form name="stats_form" method="post"><b>'.$stats_vhost_select[$lang].' </b><select class="settings" name="stats_vhost" onchange="javascript:document.stats_form.submit();">
		<option value="null">'.$vhost_select[$lang].'</option>
	');

while (array_keys($vhosts)) {

	$vhost = key($vhosts);
	if ($_POST['stats_vhost'] == $vhost) {

			$marked_vhost="selected=\"selected\"";

		}
		else {

			unset($marked_vhost);

	}

	$html->set_body('<option value="'.$vhost.'" '.$marked_vhost.'>'.$vhost.'</option>');
	array_shift($vhosts);

}

$html->set_body('</select></form>');

if ($vhost_valid === true) {


		$db->total_messages($vhost_active);
		$total_messages = $db->result;
		$html->set_body('<h2><u>'.$stats_for[$lang].$vhost_active.'</u></h2><p style="padding-left: 10px;">
			'.$stats_messages[$lang].' <b>'.number_format($total_messages[1][total_messages]).'</b> 
				'.$stats_messages_b[$lang].'<b>'.number_format($total_messages[1][total_chats]).'</b>'.$stats_messages_c[$lang].'</b></p>
			<hr size="1" noshade="noshade" style="color: #cccccc;">');

		// get dates
		$today = date("Y-n-j");
		$yesterday = date("Y-n-j", strtotime("-1 day"));
		$last_week = date("Y-n-j", strtotime("-7 days"));
		for ($ds=0;$ds<=4;$ds++) {

			$days[$ds] = date("Y-n-j", strtotime("-$ds day"));

		}

		// Top 10
		$html->set_body('<table class="ff" cellpadding="0" cellspacing="0" style="display:inline;">
					<tr><td><b>'.$stats_top[$lang].'</b></td></tr><tr><td style="width:420px; border: 0px; vertical-align: top;">');
		foreach ($days as $current_day) {

			$i=0;
			$html->set_body('<b>'.verbose_date($current_day,$months_names,$weekdays,true).' :</b><br>');
			$db->get_top_ten($current_day,$vhost_active);
			$result = $db->result;
			foreach ($result as $entry) {
	
				$i++;
				$db->get_user_name($entry[owner_id],$vhost_active);
				$local_user = $db->result->username;
				$db->get_user_name($entry[peer_name_id],$vhost_active);
				$peer_name = $db->result->username;
				$db->get_server_name($entry[peer_server_id],$vhost_active);
				$peer_server = $db->result->server_name;
				$html->set_body('
					&nbsp;<b>'.$i.'.</b> '.htmlspecialchars($local_user).'@'.$vhost_active.'<b> --> </b>
					'.htmlspecialchars($peer_name).'@'.htmlspecialchars($peer_server).' (<i><b>'.$entry[count].'</b></i>)<br>
					');

			}
			$html->set_body('<br>');

		}
		$html->set_body('</td></tr></table>');

		// get data for graphs. We can now draw data only if we have full array. This is known issue.
		$db->get_monthly_stats($vhost_active);
		$result = $db->result;

		if (count($result)<30) { 

				$html->status_message($stats_not_eno[$lang]);
		
			} 
		else { 
	

				foreach ($result as $entry) {

					$i++;
					$f[$i] = $entry[time_unix];
					$d[$i] = $entry[messages];
					$e[$i] = $entry[users_total];
	
				}

				// hourly stats
				$db->get_hourly_stats($yesterday,$vhost_active);
				$result = $db->result;
				foreach ($result as $entry) {

					$hs[$entry[hour]] = $entry[value];
				}

				// weekly stats
				$db->get_weekly_stats($last_week,$yesterday,$vhost_active);
				$result = $db->result;
				foreach ($result as $entry) {
	
					$idx++;
					$hy[$idx] = $entry[value];

				}

				$html->set_body('<table class="ff" cellpadding="0" cellspacing="0" style="display:inline;">
					<tr>
					<td style="padding-left: 10px vertical-align: top;">
					<div id="no_users" style="width:800px;height:200px;"></div><br>
					<div id="no_messages" style="width:800px;height:200px;"></div><br>
					<div id="hourly_yesterday" style="width:800px;height:200px;"></div><br>
					<div id="hourly_week" style="width:800px;height:200px;"></div>
					</td>
					</table>
					<script id="source" language="javascript" type="text/javascript">
					$(function () {

  					  var d1 = [

					');

				$cn=31;
				for ($z=1;$z<31;$z++) {

					$cn--;
					$html->set_body("[$f[$cn],$e[$cn]],");
			
				}

				$html->set_body('


					];

   					 var d2 = [

				');

				$cn=31;
				for ($z=1; $z<31; $z++) {
		
					$cn--;
					$html->set_body("[$f[$cn],$d[$cn]],");
			
				}

				$html->set_body('


					];

  					var d3 = [

				');
	
				for ($z=0;$z<24;$z++) {
		
					$html->set_body("[$z,$hs[$z]],");
			
				}

				$html->set_body('

					];

   					var d4 = [
				');

				$idx=0;
				for ($z=0;$z<168;$z++) {
		
					$idx++;
					$html->set_body("[$z,$hy[$idx]],");
				}

				$html->set_body('

					];
    
			 	   $.plot($("#no_users"), [{

						xaxis: { 
							mode: "time" 
						},
						color: "#ff0000",
						label: "'.$stats_graph1[$lang].'", 
						data: d1,
						shadowSize: 10,
						lines: { show: true, fill: true },
						points: { show: true, fill: true, radius: 3}
					}]);

				    $.plot($("#no_messages"), [{ 

						xaxis: { mode: "time" },
						color: "#3480ff",
						label: "'.$stats_graph2[$lang].'",
						shadowSize: 10, 
						data: d2,
						lines: { show: true, fill: true },
						points: { show: true, fill: true, radius: 3}
				
					}]);
    
				    $.plot($("#hourly_yesterday"), [{

						color: "#ff0000",
						label: "'.$stats_graph3[$lang].' ('.$yesterday.')", shadowSize: 10, data: d3,
						bars: { show: true }
					}]);
				
				    $.plot($("#hourly_week"), [{

						color: "#3480ff",
						label: "'.$stats_graph4[$lang].' ('.$last_week.' - '.$yesterday.')", shadowSize: 10, data: d4,
						bars: { show: true }
				
					}]);

					});

					</script>

				');

		}

}

require_once("footer.php");
?>
