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

$search_phase = $_POST['query'];

$next_link = $_GET['a']; // for pagination
$predefined = $_GET['b']; // for predefined

if ($predefined) { 

		if($enc->decrypt_url($predefined) === true) {

				$search_phase = "from:".$enc->jid;

			}
			else {

				unset($search_phase);
				unset($predefined);
		}
		
}

if ($next_link) { 

		if($enc->decrypt_url($next_link) === true) {

				$tslice_next = $enc->tslice;
				$search_phase = $enc->search_phase;
				$offset_arch = $enc->offset_arch;
				$offset_day = $enc->offset_day;
				$tag_count = $enc->tag_count;

			}
			else {
				
				unset ($next_link);
				unset ($search_phase);
		}

}

if ($tag_count=="t") { 
	
		$start_from = $offset_day; 
		
	}
	else{

		$start_from = null;
	}

$plain_phase = $search_phase; // fix me

require_once("upper.php");

//need to initialize counter here
$r=0;

// Check if user have set up OwnName
$db->get_own_name();
if ($db->result->own_name) {

		$own_name = $db->result->own_name;

	}
	else{

		$own_name = false;

}

// we need to rewrite this part internaly...
if ($search_phase!="") { 

	// check if we are using parametrized search or not
	$qquery = is_query_from($search_phase);

	// parametric search
	if ($qquery[from] == "t") {

			$user_chat_search = "1"; // temp hack
			unset($search_phase);
			list($user_name, $server) = split("@", $qquery[talker]);
			$db->get_user_id($user_name);
			$user_name = $db->result->user_id;
			$db->get_server_id($server);
			$server = $db->result->server_id;
			$search_phase=$qquery[query];

	}


	if ($search_phase) {

			if ($db->create_search_results_table() === false) {

						$html->alert_message($oper_fail[$lang]);
				}

				else {

						$score="score";
			}
	}


	//main table
	if ($time2_start AND $time2_end) {

			$html->status_message($search_warn[$lang].': '.$time2_start.' - '.$time2_end);
	
		}
		else {

			$time2_start = null;
			$time2_end = null;
	}

	$html->set_overview('<h2>'.$search_res[$lang].'</h2><table align="center" border="0" cellspacing="0" class="ff">
			<tr class="header"><td width="150">'.$time_t[$lang].'</td><td>'.$talks[$lang].'</td><td>'.$thread[$lang].'</td><td>'.$score.'</td></tr>
			<tr class="spacer"><td colspan="4"></td></tr>'
			);

	if ($offset_arch) { 
		
			$type_p="6"; 
		
		} 
		else { 
		
			$type_p="1"; 
		
	}

	// run optimized query in specyfic conditions
	if ($qquery[words]=="t" AND $qquery[from]=="t") { 

			$type_p="8"; 
		
	}

	if ($type_p==="1" OR $type_p==="8") {

			$db->get_uniq_chat_dates($time2_start, $time2_end, false, $offset_arch, $user_name,$server);

		}
		elseif($type_p==="6") {

			$db->get_uniq_chat_dates($time2_start, $time2_end, true, $offset_arch, $user_name,$server);

	}

	debug(DEBUG,"Selected search type: $type_p (first stage)");	

	$result = $db->result;

	// query offsets init for calculations:
	$arch_table = count($result);
	$external=0;
	$internal = $offset_day;

	// set string for searching
	if ($search_phase) {
	
		$db->set_user_query($search_phase);

	}

	foreach($result as $entry) {

		$external++;
		$time_slice = $entry["at"];
			
		// sub query

		if ($search_phase) {

				$type="4";

		}

		if ($user_chat_search) {

				if ($qquery['words']=="t") { 
					
						$type="5"; 

					}
					
					elseif($qquery['words'] == "f") { 
				
						$type="7"; 
				}

		}

		$a++;
		
		if ($type==="4") {

				$db->search_query($time_slice);

			}
			elseif($type==="5") {

				$db->search_query_in_user_chat($user_name,$server,$time_slice,$start_from);

			}
			elseif($type==="7") {

				$db-> search_query_chat_stream($user_name,$server,$time_slice,$start_from);

		}

		debug(DEBUG,"Selected search type: $type (second stage)");

		$search_result = $db->result;		
		$num_rows = count($search_result);

		$day_mark=0;
		if ($num_rows!="0") {

				foreach ($search_result as $results) {

					// if there is no "from:" clausule perform normal search
					if ($type!="7") {
			
							$body = base64_encode($results[body]);

							if ($db->insert_data_to_result_table (
								$results[ts],
								$time_slice,
								$results[peer_name_id],
								$results[peer_server_id],
								$results[direction],
								$body,
								$results[score],
								$results[ext]
								) === false ) {

						
									$html->alert_message($oper_fail[$lang]);

							}

						} 

						else {
			
							$internal++;
							$day_mark++;

							// we like colors dont we?
							if ($results["direction"] == "to") { 
							
									$col="e0e9f7"; 
								} 
								
								else { 
								
									$col="e8eef7";  
									
							}

							$to_user = $results["peer_name_id"];
							$to_server=$results["peer_server_id"]; 
				
							// let's make a link
							$to_base = $enc->crypt_url("tslice=$time_slice&peer_name_id=$to_user&peer_server_id=$to_server");

							// time calc
							$pass_to_next = $results["ts"];
							$new_d = $results["ts"];
							$time_diff = abs((strtotime("$old_d") - strtotime(date("$new_d"))));
							$old_d = $pass_to_next;
							
							// split line
							if ($time_diff>$split_line AND $day_mark>1 AND $type!="4") { 
					
								$in_minutes = round(($time_diff/60),0);
								$html->set_body('<tr class="splitl">
									<td colspan="5" style="font-size: 10px;">'.verbose_split_line($in_minutes,$verb_h[$lang],$in_min[$lang]).'<hr size="1" noshade="noshade" style="color: #cccccc;"></td></tr>');
							}

							// talker and server names
							$db->get_user_name($results[peer_name_id]);
							$talk = $db->result->username;
							$db->get_server_name($results[peer_server_id]);
							$sname = $db->result->server_name;
							// cleaning username
							$jid = htmlspecialchars($talk);
				
							$html->set_body('<tr id="pretty" title="'.$jid.'@'.htmlspecialchars($sname).'" 
									style="cursor: pointer;" bgcolor="'.$col.'" 
									onclick="window.open(\''.$view_type.'?a='.$to_base.'\');" 
									onMouseOver="this.bgColor=\'c3d9ff\';" 
									onMouseOut="this.bgColor=\'#'.$col.'\';">
									<td width="120" style="text-align: center;">'.$results["ts"].'</td>
									');

							// username from user roster
							$talk = query_nick_name($ejabberd_roster,$talk,$sname);

							// if there is no user in roster - advise that
							if ($talk=="f") { 
							
									$talk=$not_in_r[$lang]; 
							
							}

							// threaded view
							if ($results["direction"] == "from") { 
									
									$out=$talk;
									$tt=$tt+1;
									$aa=0;
								} 
								else { 
									$out = TOKEN;
									$aa=$aa+1;
									$tt=0;
									
							}

							if ($aa<2 AND $tt<2) { 

									$html->set_body('<td style="text-align: left;">&nbsp;');

									if ($out === TOKEN) {

											if ($own_name !== false) {

													$html->set_body(cut_nick(htmlspecialchars($own_name)));

												}
												else{

													$html->set_body(cut_nick(htmlspecialchars($out)));

											}
										}
										else {

											$html->set_body(cut_nick(htmlspecialchars($out))); 

									}

									$html->set_body('&nbsp;&nbsp;</td>');

								}
								else {
							
									$html->set_body('<td style="text-align: right;"> -</td>');

							}

							// end threaded view

							// message body
							$body_message=wordwrap(str_replace("\n","<br>",htmlspecialchars($results["body"])),107,"<br>",true);
							$html->set_body('<td width="700">'.$body_message.'</td>');

							// run pagination code only if search contains from: clausule
							/*
							The pagination code havent been changed after upgrade to search_engine_v2 - it work well so if one want to improve it
							f.e. by adding "back" button be my guest...current code is nightmare :/
							*/
							$r=$r+1;
							debug(DEBUG,"all: $r, num_r: $num_rows, internal: $internal");
							if ($r==$num_search_results) { 

									if ($num_rows>$internal) { 
								
										debug(DEBUG,"-->more results in this day...$entry[at] offset: $internal");
										$tag_count="t";
									}

									$next_r=$external+$offset_arch;
									debug(DEBUG,"before cutdown: $next_r");
									// back to one day and continue with offset
									if ($tag_count=="t") { 
									
										$next_r=$next_r-1; 
											
									}

									debug(DEBUG,"after cutdown: $next_r");
									debug(DEBUG,"Internal: $internal, offset: $offset_day, is_tag: $s_variables[tag_count]");
									// if the same day - we increase offset
									if ($internal==$offset_day AND $s_variables[tag_count] == "t") {

										$internal=$internal+$offset_day; 
										debug(DEBUG,"Increasing offset...");
							
									}
									
									// hack
									if ($qquery[from] == "t") { 
							
										$plain_phase=str_replace("@","//",$plain_phase);  
								
									}

									$trange = $enc->crypt_url("time_start=$time2_start&time_end=$time2_end");
									$lnk_n = $enc->crypt_url("tslice=$entry[at]&offset_arch=$next_r&offset_day=$internal&search_phase=$plain_phase&tag_count=$tag_count");

									$html->set_body('<tr class="spacer"><td colspan="4"></td></tr>
											<tr class="maint" style="background-image: url(img/bar_new.png); background-repeat:repeat-x; font-weight: bold; color: #fff;">
											<td colspan="2" style="text-align: left;">
											<a href="search_v2.php?a='.$lnk_p.'"></a></td>
											<td colspan="2" style="text-align: right;">
											<a href="search_v2.php?a='.$lnk_n.'&c='.$trange.'">'.$search_next[$lang].'</a></td></tr>
											');
									break 2;
					
							}

			

					}


				}

			}
		
			else{

				// if we haven't found anything increase counter by one...
				$b++;
		}

		$start_from = null; // reset...
		$internal=0;
		$day_mark=1;
		if ($num_rows!=0 AND $type=="7") { 

			if ($arch_table == $external) {
					
					$html->set_body('<tr height="6" class="spacerb"><td colspan="3" style="text-align: center;"><small>'.$no_more[$lang].'</small></td></tr>');
				}

			elseif($type=="7") {

					$html->set_body('<tr height="6" class="spacer"><td colspan="3" style="text-align: center;"><small>'.$nx_dy[$lang].'</small></td></tr>');

					// initialize thread
					$aa="0";
					$tt="0";
			}
		}

	// end of main loop
	}

	// if normal search:
	if ($type!="7" AND $type!==NULL) {
		
		$db->get_search_results();
		$result = $db->result;
		$num_results = count($result);
		$html->set_body('<tr class="maint"><td colspan="4" style="text-align: center; font-weight: normal;">'.$search_tip[$lang].' <b>'.$num_results.'</b>'.$search_why[$lang].'</td></tr>
				<tr class="spacerb"><td colspan="5"></td></tr>');
	
		foreach ($result as $dat) {

			//building link:
			$to_base = $enc->crypt_url("tslice=$dat[time_slice]&peer_name_id=$dat[peer_name_id]&peer_server_id=$dat[peer_server_id]");

			// get the name of user that we was talking to
			$db->get_user_name($dat[peer_name_id]);
			$talk = $db->result->username;

			// get it's server name
			$db->get_server_name($dat[peer_server_id]);
			$sname = $db->result->server_name;

			// cleanup jid
			$jid = htmlspecialchars($talk);

			// color every second line...
			if ($col=="e0e9f7") { 
					
					$col="e8eef7"; 
					
				} 
				else { 
				
					$col="e0e9f7"; 
					
			}

			// get username from user roster:
			$talk = query_nick_name($ejabberd_roster,$talk,$sname);

			// if user is not in list, advise about that
			if ($talk === "" ) { 
	
				$talk=$not_in_r[$lang]; 
			
			}

			// now we want to know who was talking to who...
			if ($dat["direction"] == "to") { 
	
					$fr=$to_u[$lang]; 
			
				} 
				else { 
		
					$fr=$from_u[$lang]; 
			
			}

			// ... and what was talking, and format that ...
			$body_talk = wordwrap(str_replace("\n","<br>",htmlspecialchars(base64_decode($dat["body"]))),107,"<br>",true);

			// advise user if chat is deleted. Extension=1 stands for "Chat temporary deleted" or "Chat awaiting deletion"
			if ($dat[ext] == 1) {

				$html->set_body('<tr bgcolor="b5b5b5"><td colspan="4" style="text-align: center; font-weight: bold;">'.$marked_as_d[$lang].'</td></tr>');
	
			}

			// opening line 
			if ($dat[ext]!=1) {
			
					$html->set_body('<tr id="pretty" title="'.$jid.'@'.htmlspecialchars($sname).'" 
						style="cursor: pointer;" bgcolor="'.$col.'" onclick="window.location=\''.$view_type.'?a='.$to_base.'\'" 
						onMouseOver="this.bgColor=\'c3d9ff\';" onMouseOut="this.bgColor=\'#'.$col.'\';">');
		
				}
				else {
		
					$html->set_body('<tr id="pretty" title="'.$jid.'@'.htmlspecialchars($sname).'" style="cursor: pointer;" bgcolor="b5b5b5" onclick="window.location=\'trash.php\'">');
		
			}

			// content
			$html->set_body('<td width="120" style="text-align: center;">'.$dat["ts"].'</td>
					<td style="text-align: left;">'.$fr.'&nbsp;&nbsp;'.cut_nick($talk).'&nbsp;&nbsp;</td>
					<td width="700">'.$body_talk.'</td>
					<td style="text-align: center;">'.round($dat[score],2).'</td>
					</tr>'
				);

		}

		$html->set_body('<tr class="spacer" height="1px"><td colspan="5"></td></tr><tr class="foot" height="15px"><td colspan="5"></td></tr>');

	}

	if($a==$b) { 

		$html->set_body('<tr><td colspan="4" style="text-align: center;"><b>'.$no_result[$lang].'</b></td></tr>');
		
	}

	$html->set_body('</table>');

	}

	else {

		// if user input is empty:
		$html->set_body('<br><br><center><b>'.$search1[$lang].'</b></center><br><br><br><br>');

}

require_once("footer.php");
?>
