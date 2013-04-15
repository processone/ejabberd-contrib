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

// fetch some date from encoded url...
$e_string = $_GET['a'];
$resource_id = $_GET['b'];
$start = $_GET['start'];

if ($_GET[a]) {

	$jump_link = "&a=".$_GET['a'];

}

$html->set_overview('<h2>'.$archives_t[$lang].'</h2><small>'.$cal_notice[$lang].'. <a href="calendar_view.php?set_pref=1&amp;v=2'.$jump_link.'"><u>'.$change_view_cal[$lang].'</u></a></small>');

if ($enc->decrypt_url($e_string) === true) {

		$tslice = $enc->tslice;
		$talker = $enc->peer_name_id;
		$server = $enc->peer_server_id;
		$action = $enc->action;
		$lnk = $enc->lnk;
		$e_string = $enc->crypt_url("tslice=$tslice&peer_name_id=$talker&peer_server_id=$server");
	}
	else{

		unset($e_string);

}

$html->set_body('
	<script language="javascript" type="text/javascript">

	// prepare the form when the DOM is ready 
	$(document).ready(function() { 
    		// bind form using ajaxForm 
    		$(\'#fav_form\').ajaxForm({ 
        		// target identifies the element(s) to update with the server response 
        		target: \'#fav_result\', 
 
        		// success identifies the function to invoke when the server response 
        		// has been received; here we apply a fade-in effect to the new content 
        		success: function() { 
            			$(\'#fav_result\').fadeIn(\'slow\'); 
        		}	 
    		}); 
	});
	</script>

	');

// undo delete
if ($action === "undelete") {

		if ($db->move_chat_from_trash($talker,$server,$tslice,$lnk) === true) {

				$html->status_message($undo_info[$lang],"message");

			}

			else {

				unset($talker);
				$html->alert_message($oper_fail[$lang],"message");

		}

}

// set idx
if ($_GET['idx']) {

	$idx = $_GET['idx'];

	if ($enc->decrypt_url($idx) === true) {

			if($db->set_ext_index($enc->single) !== true) {

				echo $enc->single;exit;
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

if ($action === "delete") {

		if ($db->move_chat_to_trash($talker,$server,$tslice,$lnk) === true) {

				$undo = $enc->crypt_url("tslice=$tslice&peer_name_id=$talker&peer_server_id=$server&lnk=$lnk&action=undelete");
				unset($talker);
				$idx = $enc->crypt_url("single=".$db->get_last_idx()."");
				$html->set_body('<center><div style="background-color: #fad163; text-align: center; width: 240pt;">'.$del_moved[$lang]
						.'<a href="'.$view_type.'?a='.$undo.'&amp;idx='.$idx.'"> <span style="color: blue; font-weight: bold;"><u>Undo</u></span></a></div></center>');

			}

			else {

				$html->alert_message($oper_fail[$lang],"message");
				unset($talker);

			}
		
}

// some validation things...
if ($start) { 

	if ((validate_start($start))!==true) { 
	
		$start="0";  
		
	}  
	
}

$db->get_user_stats_drop_down();
$result = $db->result;

if (count($result) !=0) {

		// main table
		$html->set_body('<br><br><table class="ff" border="0">
				<tr class="main_s"><td colspan="1" style="text-align:left;">'.$main_date[$lang].'</td>
			');
		if ($tslice) { 
		
			$html->set_body('<td>'.$talks[$lang].'</td>');
		}
		if ($talker) { 
		
			$html->set_body('<td>'.$thread[$lang].'</td>');
			
		}

		$html->set_body('<tr><td valign="top"><table border="0" class="ff"><tr><td rowspan="3" valign="top"><ul id="treemenu2" class="treeview" style="padding: 0px;">');

		foreach ($result as $entry) {

			if ($entry[at_send]==substr($tslice,0,7)) { 
		
					$rel="open"; $bop="<b>"; $bcl="</b>"; 
			
				} 
				else { 
			
					$rel=""; 
					$bop=""; 
					$bcl=""; 
				
			} 
		
			$html->set_body('<li>'.$bop.verbose_date($entry[at],$months_names,$weekdays,false,true).$bcl.'<ul rel="'.$rel.'">'); 
			$db->get_folder_content($entry[at_send]);
			$result2 = $db->result;
			foreach($result2 as $ent) {
				
				$to_base = $enc->crypt_url("tslice=$ent[at]");
				if ($tslice==$ent["at"]) { 
					
						$bold_b = "<b>"; 
						$bold_e="</b>"; 
					} 
					else { 
					
						$bold_b=""; 
						$bold_e=""; 
					
				}
			
				$html->set_body('<li><a href="?a='.$to_base.'">'.$bold_b.verbose_date($ent["at"],$months_names,$weekdays,true).$bold_e.'</a></li>');

			}

  			$html->set_body('</ul></li>');

		}

		$html->set_body('

			</ul>

			<script type="text/javascript">
				ddtreemenu.createTree("treemenu2", false, 1)
			</script>

			');

		$html->set_body('</td></tr></table>');

	}
	else {

		$html->status_message($no_archives[$lang]);
}

// Chats in selected days:
if ($tslice) {

        $db->get_user_chats($tslice);
        $result = $db->result;
	if (count($result)>0) {

			$display_conversations = true;

		}
		else{

			$display_conversations = false;

	}

if ($display_conversations === true) {

        // we need to sort list by nickname so we need to combiet 2 results: roster and mod_logdb chatlist:
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

        // sort list
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
			<table class="ff">');
	foreach ($sorted_list as $entry) {
		
		$user_name = $entry[username];
		$server_name = $entry[server_name];
		if ($talker == $entry["todaytalk"] AND $server==$entry[server]) { 
		
				$bold_b="<b>"; $bold_e="</b>"; 
				
			} 
			else { 
			
				$bold_b=""; 
				$bold_e=""; 
				
		}
			
		$nickname = $entry[roster_name];
		if (!$nickname) { 
		
				$nickname=$not_in_r[$lang]; 
				unset($malpa);
				$calday_class="caldays4";
				$spec_con = '<br><span style="text-indent: 10px; font-size: smaller;">(<i>'.htmlspecialchars($server_name).'</i>)</span>';
			
			}
			else{
			
				$malpa = "@";
				unset($spec_con);
				unset($calday_class);

		}
		
		$to_base2 = $enc->crypt_url("tslice=$tslice&peer_name_id=$entry[todaytalk]&peer_server_id=$entry[server]");
		$html->set_body('<tr>
				<td><a class="'.$calday_class.'" id="pretty" href="?a='.$to_base2.'" title="JabberID:;'.htmlspecialchars($user_name).$malpa.htmlspecialchars($server_name).'">
						'.$bold_b.cut_nick($nickname).$bold_e.'</a>');
		if ($spec_con) {

			$html->set_body($bold_b.$spec_con.$bold_e);
		}

		$html->set_body('</td></tr>');
	}

	$html->set_body('</table></td>');

	}
}

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
	$html->set_body('<table id="maincontent" border="0" cellspacing="0" class="ff"><tr><td colspan="4"><div id="fav_result"></div></td></tr>');
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
		$html->set_body('<tr><td colspan="4"><div style="background-color: #fad163; text-align: center; font-weight: bold;">'.$resource_warn[$lang].cut_nick(htmlspecialchars($res_display)).'. ');
		$html->set_body($resource_discard[$lang].'<a class="export" href="?a='.$e_string.'">'.$resource_discard2[$lang].'</a></div></td></tr>');

	}
	
	$action_link = $enc->crypt_url("tslice=$tslice&peer_name_id=$talker&peer_server_id=$server&lnk=$e_string&action=delete");
	
	$html->set_body('<tr style="background-image: url(img/bar_bg.png); background-repeat:repeat-x;">
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
					<input class="fav_main" type="submit" value="'.$fav_add[$lang].'">
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
	
	$html->set_body('
			<a id="pretty" title="'.$tip_export[$lang].'" class="foot" href="export.php?a='.$e_string.'">'.$export_link[$lang].'</a>&nbsp; | &nbsp;
			<font color="#65a5e4">'.$all_for_u[$lang].'</font>
        		<a id="pretty" title="'.$all_for_u_m2_d[$lang].'" class="foot" href="chat_map.php?chat_map='.$predefined.'"><u>'.$all_for_u_m2[$lang].'</u></a>
			&nbsp;<small>|</small>&nbsp;
			<a id="pretty" title="'.$all_for_u_m_d[$lang].'" class="foot" href="search_v2.php?b='.$predefined.'"><u>'.$all_for_u_m[$lang].'</u></a>
			&nbsp; | &nbsp;
			<a id="pretty" title="'.$tip_delete[$lang].'" class="foot" href="main.php?a='.$action_link.'">'.$del_t[$lang].'</a></td></tr>
			<tr class="spacer"><td colspan="6"></td></tr>
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
				$in_min[$lang]
			);

	// Sent all data to parsing function (message processor)
	if (message_processor($tslice,$server_name,$start,$nickname,$result,$db,$html,$enc,TOKEN,$split_line,$lang_pack,$lang,$spec_mark,$e_string) !== true) {

			$html->alert_message($oper_fail[$lang]);
			$html->destroy_content();

	}

 	$html->set_body('</tbody><tr class="spacer" height="1px"><td colspan="6"></td></tr><tr style="background-image: url(img/bar_bg.png); background-repeat:repeat-x;"><td style="text-align: center;" colspan="9">');

	for($i=0;$i < $nume;$i=$i+$num_lines_bro){

		if ($i!=$start) {
		
	    			if ($resource_id) { 
				
						$add_res="&b=$resource_id"; 

					} 
					else { 

						$add_res=""; 

				}
            			
				$html->set_body('<a href="?a='.$e_string.$add_res.'&start='.$i.'"> <b>['.$i.']</b> </font></a>');

	    		}
	    		else { 

				$html->set_body(' -'.$i.'- ');
		}

    	}

	$html->set_body('</td></tr>');
	// limiting code - end

	if (($nume-$start)>40) { 

		$html->set_body('<tr><td colspan="6" style="text-align: right; padding-right: 5px;"><a href="#top"><small>'.$back_t[$lang].'</small></a></td></tr>');

	}

	$html->set_body('</table></tr></table></td>');
}

$html->set_body('</td></tr></table>');

require_once("footer.php");
?>
