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

$html->set_body('<h2>'.$logger_overview[$lang].'</h2>');

if ($_GET[a]) {

	$offset_start=$_GET[a];

	if (!ctype_digit($offset_start)) { 
	
			unset($offset_start); 
			
		}
	
	}

if (isset($_POST['e']) AND isset($_POST['l'])) {

		$event_id=$_POST['e'];
		$level_id=$_POST['l'];
	
	}
	else{

		$event_id=$_GET['e'];
		$level_id=$_GET['l'];
	
	}

if (!ctype_digit($event_id) AND !ctype_digit($level_id)) { 

		unset($level_id); 
		unset($event_id); 
		
}

$html->set_body('<div align="center">
		<form method="post" action="logger.php">
		<select name="e" class="cc3">
		<option value="none">--- select event ---
	');
$html->set_body('<option value="1"');
if ($event_id=="1") { 

	$html->set_body(' "selected"');
} 

$html->set_body(' >Login<option value="2"');

if ($event_id=="2") { 

	$html->set_body('"selected"');

} 
$html->set_body(' >Logout<option value="3"');
if ($event_id=="3") { 

	$html->set_body('"selected"');

} 
$html->set_body(' >Login failed<option value="4"');
if ($event_id=="4") { 
	
	$html->set_body('"selected"');
} 
$html->set_body(' >Chat deletion<option value="5"');
if ($event_id=="5") { 

	$html->set_body('"selected"');
} 
$html->set_body('>Entire archive deletion<option value="6"');
if ($event_id=="6") { 

	$html->set_body('"selected"');

} 
$html->set_body( ' >Turn off archivization<option value="7"');
if ($event_id=="7") { 

	$html->set_body('"selected"');
	
} 
$html->set_body( ' >Turn on archivization<option value="8"');
if ($event_id=="8") { 

	$html->set_body('"selected"');
	
} 

$html->set_body(' >Chat exports
		</select>&nbsp;
		<select name="l" class="cc3">
		<option value="none">--- select level ---
	');
$html->set_body('<option value="1"');
if ($level_id=="1") { 

	$html->set_body('"selected"');

} 
$html->set_body('>normal<option value="2"');
if ($level_id=="2") { 

	$html->set_body('"selected"');

} 
$html->set_body('>warning<option value="3"');
if ($level_id=="3") { 

	$html->set_body('"selected"');
	
} 
$html->set_body('>alert</select>
		<input type="submit" name="filter_commit" value="Filter">
		</form>
		</div>
		<center>
		<table id="maincontent" class="ff" align="center" border="0" colspan="0" cellspacing="0" >
		<tr class="header"><td style="padding-left: 5px; padding-right: 0px;">'.$logger_f1[$lang].'</td>
		<td style="padding-left: 0px; padding-right: 10px;">'.$logger_f2[$lang].'</td>
		<td style="padding-left: 0px; padding-right: 10px;">'.$logger_f3[$lang].'</td>
		<td style="padding-left: 0px; padding-right: 10px;">'.$logger_f4[$lang].'</td></tr>
		<tr class="spacer"><td colspan="4"></td></tr>
		<tbody id="searchfield">
	');

if (!$offset_start) { 
		
		$offset_start="0"; 
		
	}

if ($event_id === "none") {

		$event_id = null;
	
	}
if ($level_id === "none") {

		$level_id = null;

	}

$db->get_num_events($event_id,$level_id);
$nume = $db->result->cnt;

if ($offset_start>$nume) { 

		$offset_start="0"; 
		
	}

$db->logger_get_events($event_id,$level_id, $offset_start,$lang);
$result = $db->result;

foreach ($result as $results) {

	if ($results[id_event]=="1" OR $results[id_event]=="3") { 
			
			$ip_desc=$logger_f_ip[$lang]; 
		
		} 
		else {

			$ip_desc=""; 
			
	}
	if ($results[id_level] == "3") { 
	
			$col="main_row_b"; 
			$f_color="style=\"color: red;\""; 
			
		} 
		else { 
		
			$col="main_row_a"; 
			$f_color=""; 
			
	}
	$html->set_body('<tr class="'.$col.'" '.$f_color.'><td style="padding-left: 0px; padding-right: 10px;">'.$results[event].'</td>
			<td>'.$results[log_time].'</td>
			<td style="text-align: center;">'.$results[level].'</td>
			<td style="padding-left: 5px;">'.htmlspecialchars($ip_desc.$results[extra]).'</td></tr>
		');

}

$html->set_body('</tbody>');

// pagination
$html->set_body('<tr class="spacer" height="1px"><td colspan="4"></td></tr><tr class="foot"><td style="text-align: center;" colspan="4">');

for($i=0;$i < $nume;$i=$i+300){

	if ($i!=$offset_start) {

			if (isset($event_id)){
				
					$e="&e=$event_id";
				}
			if (isset($level_id)){
				
					$l="&l=$level_id";
				}
		
			$html->set_body('<a href="?a='.$i.$e.$l.'"> <b>['.$i.']</b> </font></a>');
	
		}
	    
	    	else { 

	    		$html->set_body(' -'.$i.'- ');
	}
	
}

$html->set_body('</td></tr></table></center>');
require_once("footer.php");
?>
