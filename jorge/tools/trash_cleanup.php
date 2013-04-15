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


///////
///// Run this script daily, but before, set it up below
///////

////
////// NOTICE: You must set it for per-vhost basis. So for every vhost - one script. Sorry.

print date("d-m-Y H:i:s")." --> [jorge] Cleaning up trash.......";
$conn=mysql_connect("_MYSQL_HOST_", "_USER_", "_PASSWORD_") or die ("DB connect failed\n");
mysql_select_db ("_DB_NAME_") or die ("DB select failed\n");
$xmpp_host="_YOUR_XMPP_HOST_";

$query="select owner_id, peer_name_id,peer_server_id,idx,date as tslice,vhost from pending_del where timeframe < date_format((date_sub(curdate(),interval 1 month)), '%Y-%c-%e')";
$result=mysql_query($query);

if (mysql_num_rows($result)>0) {

	$i=0;

	while($row=mysql_fetch_array($result)) {

		$i++;
		$ch_del="delete from 
				`logdb_messages_$row[tslice]"."_$xmpp_host` 
			where 
				owner_id='$row[owner_id]' 
			and 
				peer_name_id='$row[peer_name_id]' 
			and 
				peer_server_id='$row[peer_server_id]' 
			and 
				ext = '$row[idx]'
				";
		$li_del="delete from 
				jorge_mylinks 
			where 
				owner_id='$row[owner_id]' 
			and 
				ext='$row[idx]' 
			and 
				peer_name_id = '$row[peer_name_id]' 
			and 
				peer_server_id='$row[peer_server_id]' 
			and 
				datat = '$row[tslice]'
			and 
				vhost = '$row[vhost]'
				";
		$fa_del="delete from 
				jorge_favorites 
			where 
				owner_id='$row[owner_id]' 
			and 
				ext='$row[idx]' 
			and 
				peer_name_id = '$row[peer_name_id]' 
			and 
				peer_server_id='$row[peer_server_id]'
			and
				vhost = '$row[vhost]'
				";
		$pe_del="delete from pending_del 
				where owner_id='$row[owner_id]' 
			and 
				peer_name_id = '$row[peer_name_id]' 
			and 
				peer_server_id='$row[peer_server_id]' 
			and 
				date='$row[tslice]' 
			and 
				idx='$row[idx]'
			and 
				vhost = '$row[vhost]'
				";
		mysql_query("$ch_del") or die("Error #1\n");
		mysql_query("$li_del") or die("Error #2\n");
		mysql_query("$fa_del") or dir("Error #3\n");
		mysql_query("$pe_del") or die("Error #4\n");

	}

		print "Deleted $i chats.\n";
	
	}

	else

	{

		print "Nothing to delete.\n";

	}

?>
