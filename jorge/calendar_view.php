<?
/*
Jorge - frontend for mod_logdb - ejabberd server-side message archive module.

Copyright (C) 2009 Zbigniew Zolkiewski

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

/This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

require_once("headers.php");
require_once("upper.php");

if ($_GET['a']) {

	$jump_link = "&amp;a=".$_GET['a'];

}

$html->set_overview('<h2>'.$cal_head[$lang].'</h2><small>'.$cal_notice[$lang].'. <a href="main.php?set_pref=1&amp;v=1'.$jump_link.'"><u>'.$change_view[$lang].'</u></a></small><br><br>');

if (isset($_GET['left'])) { 
	
		if ($enc->decrypt_url($_GET['left']) === true) {

				$left = $enc->tslice;

			}
			else {

				unset($left);

		}
	
}

if (isset($_GET['right'])) { 

		if ($enc->decrypt_url($_GET['right']) === true) {

				$right = $enc->tslice;

			}
			else {

				unset($left);

		}
		
}

$e_string = $_GET['a'];
$resource_id = $_GET['b'];
$start = $_GET['start'];
$jump_to = $_POST['jump_box'];

if ($jump_to!="") { 

	$mo=$jump_to; 

}

if ($mo === "jump") { 

	unset($mo); 

}

if ($enc->decrypt_url($e_string) === true) {

	$tslice = $enc->tslice;
	$talker = $enc->peer_name_id;
	$server = $enc->peer_server_id;
	$action = $enc->action;
	$lnk = $enc->lnk;
		
	// reencode string:
	$e_string = $enc->crypt_url("tslice=$tslice&peer_name_id=$talker&peer_server_id=$server");

}

// avoid unnessesary validation, actualy...
if ($tslice) {

	if (validate_date($tslice) === false) { 

			debug(DEBUG,"Date validation failed: $tslice");
			unset ($tslice); 
			unset($e_string); 
			unset($talker); 
			unset($left); 
			unset($right); 
			unset($mo); 
			unset($action); 
	
		}
		else{

			debug(DEBUG,"Date set to: $tslice");

	}

}

// some validation things...
if ($start) { 

	if ((validate_start($start))!==true) { 
	
		$start="0";  
		
	}  
	
}

// set idx
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

// undo delete
if ($action === "undelete") {

	if ($db->move_chat_from_trash($talker,$server,$tslice,$lnk,$idx) === true) {

			$html->status_message($undo_info[$lang],"message");

		}
		else {

			unset($talker);
			$html->alert_message($oper_fail[$lang],"message");

	}

}

if ($action === "delete") {

		if ($db->move_chat_to_trash($talker,$server,$tslice,$lnk) === true) {

				$undo = $enc->crypt_url("tslice=$tslice&peer_name_id=$talker&peer_server_id=$server&lnk=$lnk&action=undelete");
				unset($talker);
				$idx = $enc->crypt_url("single=".$db->get_last_idx()."");
				$html->status_message('<center><div style="background-color: #fad163; text-align: center; width: 240pt;">'.$del_moved[$lang]
						.'<a href="'.$view_type.'?a='.$undo.'&amp;idx='.$idx.'"> <span style="color: blue; font-weight: bold;"><u>Undo</u></span></a></div></center>');

			}

			else {

				$html->alert_message($oper_fail[$lang],"message");
				unset($talker);

			}
		
}

// check few condition, what we're doing...
if ($tslice!="") {

		list($y,$m,$selected) = split("-", $tslice);
		$mo="$y-$m";

	}

	else {

		if (isset($left)) {

			$mo=$left;

		}

		if (isset($right)) {

			$mo=$right;

		}

}

if (!isset($mo)) {

	$mo = date("Y-n");

}

// validate mo if fail, silently fallback to current date
if (validate_date($mo."-1") === false) { 
	
	unset ($tslice); 
	unset ($e_string); 
	unset ($talker); 
	$mo = date("Y-m");  

}

// master div
$html->set_body('<div>');

// calendar div
if ($talker) { 

		$float="left;"; 
		
	} 
	else { 
	
		$float="none;"; 
		
}

// select list
$db->get_user_stats_drop_down();
$ch_mo = $db->result;

// check if user have some chats
if (count($ch_mo)!=0) {

		$html->set_body('<div style="text-align: center; width: 200px; float: '.$float.'">
			<form id="t_jump" action="calendar_view.php" method="post" name="t_jump">
			<select style="text-align: center; border: 0px; background-color: #6daae7; color:#fff; font-size: x-small;" name="jump_box" size="0" onchange="javascript:document.t_jump.submit();">
			<option value="jump">'.$jump_to_l[$lang].'</option>
			');

		foreach($ch_mo as $result) {

			list($s_y,$s_m) = split("-",$result[at_send]);
			$sym="$s_y-$s_m";

			if ($jump_to!="" AND $sym==$mo) { 
	
					$sel_box="selected"; 
			
				} 
				else { 
		
					$sel_box=""; 
			
			}
			$html->set_body('<option value="'.$sym.'" '.$sel_box.'>'.verbose_date($result[at],$months_names,$weekdays,false,true).'</option>');

		}

		$html->set_body('</select></form>');

		// now generate calendar
		$db->get_user_stats_calendar($mo);
		$result_for_days = $db->result;

		$i=0;
		// days
		foreach($result_for_days as $result) {

			$i++;
			$days[$i] = str_replace("-","",$result[days]);

		}

		list($y,$m) = split("-", $mo);
		$html->set_body(calendar($db,$user_id,$xmpp_host,$y,$m,$days,TOKEN,$url_key,$left,$right,$selected,$lang,$view_type,1,$null_a=0,$null_b=0,$cal_days,$enc,$months_names,$weekdays));
		unset($days);

	}
	else {

		$html->status_message($no_archives[$lang]);
		
}

// if we got day, lets display chats from that day but only if there are some
if ($tslice) {

	$db->get_user_chats($tslice);
	$result = $db->result;
	if (count($result)>0) {

			$display_conversations = true;

		}
		else{

			$display_conversations = false;

	}

}

if ($display_conversations === true) {
	
	// we need to sort list by nickname so we need to combine 2 results: roster and mod_logdb chatlist:
	foreach($result as $sort_me) {
		
		$roster_name = query_nick_name($ejabberd_roster,$sort_me[username],$sort_me[server_name]);
		$arr_key++;

		if (!$roster_name) {
			
				// split contact into 2 arrays: one with full jids, second without names - transports, agents..
                        	$sorted_spec[$arr_key] = array(
                                        "roster_name"=>$roster_name,
                                        "username"=>$sort_me[username],
                                        "server_name"=>$sort_me[server_name],
                                        "todaytalk"=>$sort_me[todaytalk],
                                        "server"=>$sort_me[server],
                                        "lcount"=>$sort_me[lcount]
                                        );
			}
			else {

                		$sorted_list[$arr_key] = array(
                                	"roster_name"=>$roster_name,
                                	"username"=>$sort_me[username],
                                	"server_name"=>$sort_me[server_name],
                                	"todaytalk"=>$sort_me[todaytalk],
                                	"server"=>$sort_me[server],
                                	"lcount"=>$sort_me[lcount]
                                	);
		}

	}	
	// sort and split two lists: normal contacts and special contacts.
	asort($sorted_list);
	
	if (!$show_spec) {

		$show_spec="1";

	}

	if ($sorted_spec AND $show_spec === "1") {

		if ($sorted_list) {
		
				$sorted_list = array_merge($sorted_list,$sorted_spec);

			}
			else{

				$sorted_list = $sorted_spec;
			
		}

	}

	$html->set_body('<td valign="top" style="padding-top: 15px;">
        		<table width="200" border="0" cellpadding="0" cellspacing="0" class="calbck_con">
      			<tr>
        			<td><img src="img/cal_corn_11.png" width="15" height="7"></td>
        			<td style="background-image: url(img/cal_bck_top.gif);"></td>
        			<td><img src="img/cal_corn_12.png" width="14" height="7"></td>
      			</tr>
      			<tr>
        			<td width="15" height="226" valign="top" class="calbckleft"><img src="img/cal_bck_left.png" width="15" height="116"></td>
        			<td width="100%" valign="top">
					<table width="100%"  border="0" cellspacing="0" cellpadding="0">
        				<tr>
						<td align="center" class="calhead">'.$chat_list_l[$lang].'</td>
					</tr>
					<tr><td height="5"></td></tr>
					<tr align="center" class="caldays">
					<td><div style="vertical-align: middle; overflow: auto; height: 210; border-left: 0px; border-bottom: 0px; padding:0px; margin: 0px;">
	');
	
	// select chatters
	foreach ($sorted_list as $entry) {

                $user_name = $entry[username];
                $server_name = $entry[server_name];
                if ($talker==$entry["todaytalk"] AND $server==$entry[server]) { 

				$bold_b="<font color=\"#ffcc00\"><b>"; 
				$bold_e="</b></font>"; 
				$mrk=1; 
				
			} 
			else { 
				
				$bold_b=""; 
				$bold_e=""; 
				$mrk=0; 
				
		}
		
		$nickname = $entry[roster_name];
		if (!$nickname) { 
		
				$calday_class="caldays4";
				$nickname = $not_in_r[$lang];
				$spec_con = '<br><span style="text-indent: 10px; font-size: smaller;">(<i>'.htmlspecialchars($server_name).'</i>)</span>';
				unset($malpa);
				
			}
			else {

				$calday_class="caldays3";
				unset($spec_con);
				$malpa = "@";

		}
		
		if ($mrk==1) {

			$db->get_next_prev_day($entry[todaytalk],$entry[server],$tslice,"p");
			$previous_t = $db->result->at;
			$to_base_prev = $enc->crypt_url("tslice=$previous_t&peer_name_id=$entry[todaytalk]&peer_server_id=$entry[server]");

			$db->get_next_prev_day($entry[todaytalk],$entry[server],$tslice,"n");
			$next_t = $db->result->at;
			$to_base_next = $enc->crypt_url("tslice=$next_t&peer_name_id=$entry[todaytalk]&peer_server_id=$entry[server]");
				
		}

		$to_base2 = $enc->crypt_url("tslice=$tslice&peer_name_id=$entry[todaytalk]&peer_server_id=$entry[server]");
		if ($mrk==1 AND $previous_t != NULL) { 

				$html->set_body('<a class="nav_np" id="pretty" title="'.$jump_to_prev[$lang].': '.$previous_t.'" href="calendar_view.php?a='.$to_base_prev.'"><<< </a>');
					
		}

		$html->set_body('<a class="'.$calday_class.'" id="pretty" href="?a='.$to_base2.'" title="JabberID:;'.htmlspecialchars($user_name).$malpa.htmlspecialchars($server_name).';---;
			<b>'.$chat_lines[$lang].$entry[lcount].'</b>">'.$bold_b.cut_nick($nickname).$bold_e.'</a>');

		if ($mrk==1 AND $next_t != NULL) { 
						
			$html->set_body('<a class="nav_np" id="pretty" title="'.$jump_to_next[$lang].': '.$next_t.'" href="calendar_view.php?a='.$to_base_next.'"> >>></a>');
		}

		if ($spec_con) {

			$html->set_body($bold_b.$spec_con.$bold_e);

		}

		$html->set_body('<br>');

        }

	$html->set_body('
		</div></td></tr>
		</table>
        	</td>
        	<td width="14" valign="top" class="calbckright"><img src="img/cal_bck_right.png" width="14" height="116"></td>
      		</tr>
      		<tr>
        	<td><img src="img/cal_corn_21.png" width="15" height="16"></td>
        	<td style="background-image: url(img/cal_bck_bot.png);"></td>
        	<td><img src="img/cal_corn_22.png" width="14" height="16"></td>
      		</tr>
    		</table>
	');
}

$html->set_body('</div><div>');

// Chat thread:
if ($talker) {

        $html->set_body('<td valign="top"><table border="0" class="ff"><tr>');
        if (!$start) { 
		
			$start="0"; 
			
		}
	$db->get_num_lines($tslice,$talker,$server);
	$nume = $db->result->cnt;
        if ($start>$nume) { 
	
			$start=$nume-$num_lines_bro; 
			
		} 

	$db->get_user_name($talker);
	$talker_name = $db->result->username;
	$db->get_server_name($server);
	$server_name = $db->result->server_name;
	$nickname = query_nick_name($ejabberd_roster,$talker_name,$server_name);
        if ($nickname === "") { 
		
				$nickname=$not_in_r[$lang];
				$spec_mark = true;
			
			}
			else {

				$spec_mark = false;

	}
	
	$predefined = $enc->crypt_url("jid=$talker_name@$server_name");

	$html->set_body('<table id="maincontent" border="0" cellspacing="0" class="ff">
			<tr><td colspan="4"><div id="fav_result"></div>
			</td></tr>
		');

	if ($_GET['loc']) {

		$loc_id=$_GET['loc'];
		if ($loc_id=="2") {

				$back_link_message=$chat_map_back[$lang];
				$back_link="chat_map.php?chat_map=$predefined";
			
			}
			elseif($loc_id=="3") {
			
				$back_link_message=$fav_back[$lang];
				$back_link="favorites.php";
			
			}
			elseif($loc_id=="4") {

				$back_link_message=$myl_back[$lang];
				$back_link="my_links.php";
			
			}
		$html->set_body('<tr><td colspan="2" class="message"><a href="'.$back_link.'">'.$back_link_message.'</a></td><td></td></tr>');
	
	}
        if ($resource_id) {
		
		$db->get_resource_name($resource_id);
		$res_display = $db->result->resource_name;
        	$html->set_body('<tr><td colspan="4"><div style="background-color: #fad163; text-align: center; font-weight: bold;">'.$resource_warn[$lang].cut_nick(htmlspecialchars($res_display)).'. '
				.$resource_discard[$lang].'<a class="export" href="?a='.$e_string.'">'.$resource_discard2[$lang].'</a></div></td></tr>');
        
	}
	
	$action_link = $enc->crypt_url("tslice=$tslice&peer_name_id=$talker&peer_server_id=$server&lnk=$e_string&action=delete");
	$sess->set('export_nickname',$nickname); // pass to export
	$html->set_body('
        		<tr class="header">
        		<td><b> '.$time_t[$lang].' </b></td><td><b> '.$user_t[$lang].' </b></td><td><b> '.$thread[$lang].'</b></td>
        		<td align="right" style="padding-right: 5px; font-weight: normal;">
			');

	// check favorite
	$db->check_favorite($talker,$server,$tslice);
	if ($db->result->cnt < 1) {

			$html->set_body('
					<form style="margin-bottom: 0;" action="favorites.php" method="post">
					<input type="hidden" name="a" value="'.$_GET[a].'">
					<input type="hidden" name="init" value="1">
					<input class="fav" type="submit" value="'.$fav_add[$lang].'">
					</form>
					');
		}
		else {

			$html->set_body('
					<form style="margin-bottom: 0;" action="favorites.php" method="post">
					<input type="hidden" name="a" value="'.$_GET[a].'">
					<input type="hidden" name="init" value="1">
					<i>'.$fav_favorited[$lang].'</i>
					</form>
					');
	
		
		}
	
	$html->set_body('<a id="pretty" title="'.$tip_export[$lang].'" class="menu_chat" href="export.php?a='.$e_string.'">'.$export_link[$lang].'</a>&nbsp; | &nbsp;'.$all_for_u[$lang].'
        		<a id="pretty" title="'.$all_for_u_m2_d[$lang].'" class="menu_chat" href="chat_map.php?chat_map='.$predefined.'"><u>'.$all_for_u_m2[$lang].'</u></a>
			&nbsp;<small>|</small>&nbsp;
			<a id="pretty" title="'.$all_for_u_m_d[$lang].'" class="menu_chat" href="search_v2.php?b='.$predefined.'"><u>'.$all_for_u_m[$lang].'</u></a>
			&nbsp; | &nbsp;
        		<a id="pretty" title="'.$tip_delete[$lang].'" class="menu_chat" href="calendar_view.php?a='.$action_link.'">'.$del_t[$lang].'</a>
			</td></tr>
        		<tr class="spacer"><td colspan="7"></td></tr>
        		<tbody id="searchfield">
	');

	if($db->get_user_chat($tslice,$talker,$server,$resource_id,$start,$num_lines_bro) === false) {

			$html->alert_message($oper_fail[$lang]);

	}
	// processing messages. this should be handled as separate message_processor, so that tree view and calendar view can share the same code withoud redundancy. To be done in 2.0
	$result = $db->result;

	// some strings to pass to message_processor
	$lang_pack = array(
				$cont_chat_p[$lang],
				$message_type_message[$lang],
				$message_type_error[$lang],
				$message_type_headline[$lang],
				$resource_only[$lang],
				$muc_message[$lang],
				$my_links_save[$lang],
				$verb_h[$lang],
				$in_min[$lang],
				$cont_chat[$lang]
			);

	// Sent all data to parsing function (message processor)
	if (message_processor($tslice,$server_name,$start,$nickname,$result,$db,$html,$enc,TOKEN,$split_line,$lang_pack,$lang,$spec_mark,$e_string,$to_base_prev,$to_base_next) !== true) {

			$html->alert_message($oper_fail[$lang]);
			$html->destroy_content();

	}

	// limiting code
	$html->set_body('<tr class="spacer"><td colspan="7"></td></tr><tr class="foot"><td style="text-align: center;" colspan="9">');

	for($i=0;$i < $nume;$i=$i+$num_lines_bro){

        	if ($i!=$start) {

            			if ($resource_id) { 
			
						$add_res="&amp;b=$resource_id"; 
					} 
					else { 
			
						$add_res=""; 
				}

            			$html->set_body('<a class="menu_chat" href="?a='.$e_string.$add_res.'&amp;start='.$i.'"> <b>['.$i.']</b> </font></a>');
            		}

            		else { 
	    
	    			$html->set_body('<span style="color: #fff;">-'.$i.'-</span> '); 
		
		}
	
	}

	$html->set_body('</td></tr>');
	// limiting code - end

        if (($nume-$start)>40) { 

			$html->set_body('<tr><td colspan="6" style="text-align: right; padding-right: 5px;"><a href="#top"><small>'.$back_t[$lang].'</small></a></td></tr>');
		
	}

        $html->set_body('</table></tr></table></td>');
}

$html->set_body('</div></div>');

require_once("footer.php");
?> 
