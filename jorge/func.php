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

if (__FILE__==$_SERVER['SCRIPT_FILENAME']) {

	header("Location: index.php?act=logout");
	exit;

}

function get_user_agent($_SERVER) {
	
	if (preg_match("/Macintosh/i",$_SERVER['HTTP_USER_AGENT'])) {

			return true;

		}
		else{

			return false;

	}
	return false;

}


function getmicrotime(){

	list($usec, $sec) = explode(" ",microtime());
	return ((float)$usec + (float)$sec);

}


function query_nick_name($ejabberd_roster,$talker, $server="") {

	$nickname = $ejabberd_roster->get_nick("$talker"."@"."$server");
	if ($nickname=="") { 
	
			$nickname=$talker; 
			
		}

	return $nickname;

}


function validate_date($tslice) {

	list($ye, $mo, $da) = split("-", $tslice);
	return checkdate($mo,$da,$ye);

}


function check_registered_user ($sess,$ejabberd_rpc,$enc) {
	
	if (!$sess->is_registered('uid_l') OR !$sess->is_registered('uid_p')) {
			
			return false;
  		}
		else {

			if ($enc->decrypt_url($sess->get('uid_p')) === true) {

					$uid_p = $enc->single;

				}
				else {

					return false;

			}

			$ejabberd_rpc->set_user($sess->get('uid_l'),$uid_p);
			if ($ejabberd_rpc->auth() === true) {

					return true;

				}
				else {
			
					return false;
			}


	}

	return false;

}


function is_query_from($query) {

	list($from,$talker,$query_p) = split(":",$query);
	$from=trim($from);
	if ($from=="from") {

			$qquery[from] = "t";
			$qquery[talker] = trim($talker);
			$qquery[talker] = str_replace("//","@",$qquery[talker]); // hack for parametrized search
			if ($query_p) {
				
					$qquery[query] = $query_p;
					$qquery[words] = "t";
					return $qquery;
					
				}
				else {

					$qquery[words] = "f";
					return $qquery;
					
			}
	
		}
		else {

			// normal search
			return "f";

	}


}


function verbose_date($raw_date,$months_names = null,$weekdays = null,$t = false,$y = false) {
	
	// English calendar arrays. Here we convert names from english calendar to other language. Make sure your locale in php work with default set to english.
	$english_months = array("January","February", "March", "April", "May","June","July","August","September","October", "November","December");
	$english_days = array("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday");

	// Various formats
	if ($t === true) {

			return str_replace($english_days,$weekdays,strftime("%e.%m (%A)",strtotime("$raw_date")));
		
		}
		elseif($y === true) {

			return str_replace($english_months,$months_names,strftime("%B %Y", strtotime("$raw_date")));

		}
		else{

			return str_replace($english_months,$months_names,strftime("%e %B %Y", strtotime("$raw_date")));
	}
	
}

function validate_start($start) {

	if (!ctype_digit($start)) { 
	
		return false;
	
	}
	if (fmod($start,10)=="0") { 
	
			return true;
			
		} 
		else { 
		
			return false;
			
	}

}


function db_size() {

	$result = mysql_query("show table status");
	$size = 0;
	while($row = mysql_fetch_array($result)) {

		$size += $row["Data_length"];
	
	}

	$size = round(($size/1024)/1024, 1);
	return $size;

}


function verbose_split_line($in_minutes,$verb_h,$in_min) {

	if ($in_minutes>60) {
		return $verb_h;
	}
	elseif ($in_minutes<60)  {
		return $in_minutes." ".$in_min;
	}

}


function cut_nick($nick) {

	if (strlen($nick)> 25) {
		$nick=substr($nick,0,25)."...";

	}
	
	return $nick;
}


function new_parse_url($text) {

	$text = ereg_replace("([[:alpha:]]+://www|[[:alpha:]]+://)[^<>[:space:]]+[[:alnum:]/]",

                     "<a class=\"clickl\" href=\"\\0\" target=\"_blank\">\\0</a>", $text);

	// disabled for now
	#$text = ereg_replace("[^://]?www[^<>[:space:]]+[[:alnum:]/]",
        #            "<a class=\"clickl\" href=\"http://\\0\" target=\"_blank\">\\0</a>", $text);
	
	return $text;
}


function calendar($db,$user_id,$xmpp_host,$y,$m,$days,$token,$url_key,$left,$right,$selected,$lang,$view_type,$c_type,$name_peer=0,$server_peer=0,$cal_days=0,$enc=null,$months_names,$weekdays) {
	
	$days=$days;
	$month = $m;
	$year = $y;

//create arrays for the calendar

    $months_days = array("31","28","31","30","31","30","31","31",
                         "30","31","30","31");

	$days_array = array("Mon","Tue","Wed","Thu","Fri","Sat","Sun");

//removes the 0 from start of month - can't find array key with 0

    if(strlen($month)==1){
        $month= str_replace("0","",$month);
    }
    else{
        $month=$month;
    }

//reset month to the array key match (array starts at 0)

    $month= $month-1;

//find the days in the month

    $days_in_month = $months_days[$month];

//$m is used to find month

    $m = $month+1;

//find the first day of the month     
 
    $time = date("M D Y H:i:s", mktime(0, 0, 0, $m, 1, $year));
    $first_day = explode(" ",$time);
    $time = $first_day[1];

//create the links to next and previous months

    $next = $month+2;
    $x = $year;

//if month is 13 then new year

    if($next==13){
        $next=1;
        $x = $x+1;
    }
    $prev = $month;
    $y = $year;

//if month is 0, then previous year

    if($prev==0){
        $prev=12;
        $y=$y-1;
    }

    $calendar = "";

//Build the calendar with css
//links to next and previous month only for browser
if ($c_type=="1") {

		// encode links
		$link_left = $enc->crypt_url("tslice=$y-$prev");
		$link_right = $enc->crypt_url("tslice=$x-$next");

		// check if we have chats in prev and next mo
		$db->is_left_or_right("$y-$prev");
		$i_left = $db->result;
		$db->is_left_or_right("$x-$next");
		$i_right = $db->result;


	}
	else {

		$i_left=0;
		$i_right=0;

	}

    $calendar .='
	<table width="200"  border="0" cellpadding="0" cellspacing="0" class="calbck">
      	<tr>
        <td><img src="img/cal_corn_11.png" width="15" height="7" alt="cal_img"></td>
        <td style="background-image: url(img/cal_bck_top.gif);"></td>
        <td><img src="img/cal_corn_12.png" width="14" height="7" alt="cal_img"></td>
      	</tr>
      	<tr>
        <td width="15" valign="top" class="calbckleft"><img src="img/cal_bck_left.png" width="15" height="116" alt="cal_img">
      	</td>
        <td width="100%" valign="top">
        <table width="100%"  border="0" cellspacing="0" cellpadding="0">
        <tr>
        <td height="15" align="center" class="caldays">
	';

if ($i_left!=0) { $calendar.='<a href="?left='.$link_left.'"><<<</a>'; }

$verb_date = "$year-$m-1";

	    $calendar.='
	    	
		&nbsp;</td>
            	<td colspan="5" align="center" class="calhead">'.verbose_date($verb_date,$months_names,$weekdays,false,true).'</td>
            	<td align="center" class="caldays">&nbsp;
		
		';
	  if ($i_right!=0) { $calendar.='<a href="?right='.$link_right.'">>>></a>'; }
	    $calendar.='

	    	</td>
	  	</tr>
          	<tr align="center" class="calweek">
            	<td width="14%" height="15">'.$cal_days[$lang][1].'</td>
            	<td width="14%">'.$cal_days[$lang][2].'</td>
            	<td width="14%">'.$cal_days[$lang][3].'</td>
            	<td width="14%">'.$cal_days[$lang][4].'</td>
            	<td width="14%">'.$cal_days[$lang][5].'</td>
            	<td width="14%">'.$cal_days[$lang][6].'</td>
            	<td width="14%">'.$cal_days[$lang][7].'</td>
          	</tr>
	    
	    ';
               
    //checks for leap years and add 1 to February

    if(($year % 4 =="") && ($month==1)){
        $days_in_month=$days_in_month+1;
    }

    else{
        $days_in_month=$days_in_month;
    }

    $new_time="";
     
    //find how many blank spaces at beginning of the month
     
    foreach($days_array as $key=>$value){
     
        if($value == $time){
            $new_time .= $key+1;
        }
        else{
            $new_time .="";
        }
    }
     
    //loop through the days in the month
	$c=0;

    for($k=1;$k<($days_in_month+$new_time);$k++){   

	$c++;
	if ($c==1) { $calendar.='<tr align="center" class="caldays">'; }


 
            //blank space
     
        if($k<$new_time){
            $calendar.='<td height="15">&nbsp;</td>
            ';
            continue;
        }
         
        //start the actual days
              
        $n = $k-$new_time+1;

        if(in_array($n,$days)){
	
	if ($c_type=="1") {

			$to_base = $enc->crypt_url("tslice=$year-$m-$n");
			$loc_orign="";

		}
		elseif($c_type=="2") {

			$to_base = $enc->crypt_url("tslice=$year-$m-$n&peer_name_id=$name_peer&peer_server_id=$server_peer");
			$loc_orign="&amp;loc=2";

		}

	    if ($selected==$n) { $bgcolor = 'bgcolor="#6daae7"'; } else { $bgcolor=""; }
            $calendar .= '<td height="15" '.$bgcolor.' onclick="window.location=\''.$view_type.'?a='.$to_base.$loc_orign.'\'"><b><a class="caldays2" href="'.$view_type.'?a='.$to_base.$loc_orign.'">'.$n.'</a></b></td>
                         ';   
        }
        else{
            $calendar .= '<td height="15">'.$n.'</td>
                         ';
        }     

	if ($c==7) { $calendar.='</tr>'; $c=0; }


    }
    $calendar .= '

	</table>
        </td>
        <td width="14" valign="top" class="calbckright"><img src="img/cal_bck_right.png" width="14" height="116" alt="cal_img"></td>
      	</tr>
      	<tr>
        <td><img src="img/cal_corn_21.png" width="15" height="16" alt="cal_img"></td>
        <td style="background-image: url(img/cal_bck_bot.png);"></td>
        <td><img src="img/cal_corn_22.png" width="14" height="16" alt="cal_img"></td>
      	</tr>
    	</table>
        
	';

    return($calendar);
}


function check_thread($db,$peer_name_id,$peer_server_id,$at,$xmpp_host,$dir=NULL) {

	#adjust this hours as needed, we assume if chat is +/- 1 hour on the edge of day, then chat is related
	if ($dir=="1") {
		$day="+1 day";
		$bhour="00:00:00";
		$ehour="00:30:00";
	}
	elseif($dir=="2"){
		$day="-1 day";
		$bhour="23:30:00";
		$ehour="23:59:59";
	}

	$get_date = date("Y-n-j", strtotime($day, strtotime(date("$at"))));

	$db->check_thread($get_date,$peer_name_id,$peer_server_id,$bhour,$ehour);
	if ($db->result > 0) {

			return true;
		}
		else{
			return false;

	}

	return false;

}

function check_rpc_server($rpc_arr,$rpc_port) {

	foreach($rpc_arr as $rpc_host) {

		// assume if response time is greater then 1 second RPC server is down
		$fp=fsockopen("$rpc_host", $rpc_port, $errno, $errstr, 1);
		if ($fp!=false) {

			return $rpc_host;

		}
	
	}

	return false;
	
}


function debug($debug=false,$string) {

	if ($debug===true) {
		
		print "<small>".htmlspecialchars($string)."</small><br>";
	}

	return;

}

function message_processor($tslice,$server_name,$start,$nickname,$result_messages,$db,$html,$enc,$token,$split_line,$lang_pack,$lang,$spec_mark,$e_string,$to_base_prev,$to_base_next) {

	/* 
		This function perform message processing for message archives
		tslice - date of chat
		server_name - name of server
		start - from what point of time of day should chat be displayed
		nickname - peer nickname
		result_messages - array of messages to be parsed
		db - database object
		html - html object
		enc - encryption object
		token - owner name
		split_line - config option
		lang_pack - array of translations
		lang - language pack
		spec_mark - marker used to distinguish what we are doing
		e_string - url 
	*/

	// Check if user have set up OwnName
	$db->get_own_name();
	if ($db->result->own_name) {

			$own_name = $db->result->own_name;

		}
		else{

			$own_name = false;

	}

	// Main loop
	foreach($result_messages as $entry) {

		// always get resource_id if message is type of groupchat
		if ($entry[type] !== "groupchat") {

				if ($resource_last !== $entry[peer_resource_id]) {

					if ($db->get_resource_name($entry[peer_resource_id]) === false) {

						return false;
					}

					$resource = $db->result->resource_name;

				}
			
			}
			else{

				if ($db->get_resource_name($entry[peer_resource_id]) === false) {

						return false;
				
				}
				$resource = $db->result->resource_name;

		}

		$resource_last = $entry[peer_resource_id];
                $licz++;

		// marking messages
		if ($entry["type"] === "chat" OR $entry["type"] == "") {

                		if ($entry["direction"] === "to") { 
		
						$col="main_row_a"; 
					} 
					else { 

						$col="main_row_b"; 
				}
			}
			elseif($entry["type"] === "error") {

					$col="main_row_error";

				}
			elseif($entry["type"] === "normal") {

					$col="main_row_message";

				}
			elseif($entry["type"] === "headline") {

					$col="main_row_headline";

			}
		
                $ts = strstr($entry["ts"], ' ');
                // time calc 
                $pass_to_next = $entry["ts"];
                $new_d = $entry["ts"];
                $time_diff = abs((strtotime("$old_d") - strtotime(date("$new_d"))));
                $old_d = $pass_to_next;
                // end time calc
                if ($time_diff>$split_line AND $licz>1) { 
                                
				$in_minutes = round(($time_diff/60),0);
                                $html->set_body('<tr class="splitl">
							<td colspan="7" style="font-size: 10px;"><i>'.verbose_split_line($in_minutes,$lang_pack[7],$lang_pack[8]).'</i>
							<hr size="1" noshade="noshade" style="color: #cccccc;"></td></tr>
						');
		}

		// check if chat is continuation from previous day (work only for calendar view)
		if ($to_base_prev !== NULL) {

			if ($ts_mark!="1" AND substr($ts, 0 , strpos($ts, ":")) == 00 ) {

				if ( check_thread($db,$talker,$server,$tslice,$xmpp_host,2) === true) {
				
						$html->set_body('<tr><td colspan="6" style="text-align: left; padding-left: 5px;" class="message"><a href="calendar_view.php?a='.$to_base_prev.'">'.$lang_pack[0].'</a></td></tr>');
				}
				// check only first line
				$ts_mark="1";
			}

		}

		// run code only if type is not groupchat
		if ($entry["type"] !== "groupchat") {

			// setting subject
			if ($col==="main_row_message" OR $col==="main_row_headline") {

				if ($entry["subject"]) {

						$subject = ": ".$entry["subject"];

					}
					else{

						unset($subject);

				}
		
			}

			// add line in case of special message
			if ($col==="main_row_message") {

				$html->set_body('<tr class="main_row_message"><td colspan="7" class="main_row_special">'.$lang_pack[1].' '.htmlspecialchars($subject).'</td></tr>');

			}

			if ($col==="main_row_error") {

				$html->set_body('<tr class="main_row_error"><td colspan="7" class="main_row_special">'.$lang_pack[2].'</td></tr>');
				
			}

			if ($col==="main_row_headline") {

				$html->set_body('<tr class="main_row_headline"><td colspan="7" class="main_row_special">'.$lang_pack[3].' '.htmlspecialchars($subject).'</td></tr>');

			}

		}

		// calculate chat direction, whether to display nick...
                if ($entry["direction"] == "from") {

                                $out=$nickname;
                                $tt=$tt+1;
                                $aa=0;
                        
			}
                        else{

                                $out = $token;
                                $aa=$aa+1;
                                $tt=0;
                        
			}

		// timestamp, beginning of the chatline
		if ($entry["type"] !== "groupchat") {

                		$html->set_body('<tr class="'.$col.'"><td class="time_chat" style="padding-left: 10px; padding-right: 10px;";>'.$ts.'</td>');

			}
			else{

				if ($out!==$token) {

					// colorize
					if ($resource_group === $resource OR !$resource_group) {

							if (!$col) {

								$col = "main_row_group_to";
							
							}
							$col=$col;
						
						}
						else{

							if($col === "main_row_group_from") {

								$col="main_row_group_to";
							}
							else{

								$col="main_row_group_from";

							}
						
					}
					$html->set_body('<tr class="'.$col.'"><td class="time_chat" style="padding-left: 10px; padding-right: 10px;";>'.$ts.'</td>');

				}
		}

		// different bahaviour for groupchat and other messages
		if ($entry["type"] !== "groupchat") {
                
				if ($aa<2 AND $tt<2) {

                                		$html->set_body('<td style="padding-left: 5px; padding-right: 10px; nowrap="nowrap">');
						if ($out === TOKEN) {

								// display of Own Name
								if ($own_name !== false) {

										$html->set_body(cut_nick(htmlspecialchars($own_name)));

									}
									else{

										$html->set_body(cut_nick(htmlspecialchars($out)));

								}

							}
							else{

								$html->set_body(cut_nick(htmlspecialchars($out)));

						}

						$html->set_body('<a name="'.$licz.'"></a>');

                                		if ($out !== $token) {

								if ($spec_mark === false) {

                                						$html->set_body('
											<br><div style="text-align: left; padding-left: 5px;">
											<a class="export" id="pretty" title="'.$lang_pack[4].'" href="?a='.$e_string.'&amp;b='.$entry[peer_resource_id].'">
                                							<small><i>'.cut_nick(htmlspecialchars($resource)).'</i></small></a></div>
											');
							
									}
									else{

										$html->set_body('<br><div style="text-align: left; padding-left: 5px;">
											<small><i>'.cut_nick(htmlspecialchars($server_name)).'</i></small></div>
											');
								}

                                		}

                                		$html->set_body('</td>');
                                		$here = "1";

                        		}
                        		else {

                                		$html->set_body('<td style="text-align: right; padding-right: 5px">-</td>');
						$here = "0";
                        
				}

			}
			else{

				// do not display own chats sent to MUC. Here resource is actualy nickname as MUC standard specify
				if ($out !== $token) {

						if ($resource_group !== $resource) {

								// if message is sent without resource, that must be channel message, advise user.
								if ($resource === "") {

									$resource = $lang_pack[5];
								
								}

								$html->set_body('<td style="padding-left: 5px; padding-right: 10px; nowrap="nowrap"><small>MUC: <i>'.cut_nick($out).'</i></small><a name="'.$licz.'"></a>
                                					<br><div style="text-align: left; padding-left: 5px;">'.cut_nick(htmlspecialchars($resource)).'</div></td>
									');
								$here = "1";
								$resource_group = $resource;
						
							}
							else{

								$html->set_body('<td style="text-align: right; padding-right: 5px">-</td>');
								$here = "0";
							
							}
					
					}
					else{

						$here = "0";
					
					}
		}

		// process body part, do not show chat if message is type of groupchat
		// this is sadly funny i write this 'if' and i dont know what exacly it do :/

		if ($out !== $token OR $entry["type"] !== "groupchat") {

			// prepare body
                	$new_s = htmlspecialchars($entry["body"]);
                	$to_r = array("\n");
                	$t_ro = array("<br>");
                	$new_s = str_replace($to_r,$t_ro,$new_s);
                	$new_s = wordwrap($new_s,107,"<br>",true);
                	$new_s = new_parse_url($new_s);
                	$html->set_body('<td width="800" colspan="3">'.$new_s.'</td>');
			// generate mylink only on selected lines
                	if ($here==="1") { 

					$lnk = $enc->crypt_url("tslice=$tslice&peer_name_id=$entry[peer_name_id]&peer_server_id=$entry[peer_server_id]");
					$to_base2 = $enc->crypt_url("tslice=$tslice&peer_name_id=$entry[peer_name_id]&peer_server_id=$entry[peer_server_id]&ismylink=1&linktag=$licz&lnk=$lnk&strt=$start");
					$html->set_body('<td colspan="2" style="padding-left: 2px; font-size: 9px;"><a style="color: #1466bc" href="my_links.php?a='.$to_base2.'">'.$lang_pack[6].'</a></td>');
			
				} 
				else { 

					$html->set_body('<td></td>');
				
				}

                	if ($t=2) { $c=1; $t=0; } // WTF!?
                
			$html->set_body('</tr>');

		}

	}

	
	$html->set_body('</tbody>');

	// Check thread. Work only for calendar view.
	if ($to_base_next !== NULL) {

		if (substr($ts, 0 , strpos($ts, ":")) == 23 AND date("Y-n-j") !== $tslice) {

			if ( check_thread($db,$talker,$server,$tslice,$xmpp_host,1) === true) {
		
				$html->set_body('<tr><td colspan="6" style="text-align: right; padding-right: 5px;" class="message"><a href="calendar_view.php?a='.$to_base_next.'">'.$lang_pack[9].'</a></td></tr>');
			
			}
		}

	}

	return true;

}

function is_language_supported($l_query,$language_support,$l_key = 0, $return_val = false) {
	
	while (array_keys($language_support)) {

		$lang_key = key($language_support);
		if ($l_query === $language_support[$lang_key][$l_key]) {

			if ($return_val === true) {

					return $language_support[$lang_key][0];

				}
				else {

					return true;

			}
		
		}

		array_shift($language_support);

	}

	return false;

}


?>
