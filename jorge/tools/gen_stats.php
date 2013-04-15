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
#
# This script generates stats for message propagation. Run it once after every midnight.
# It generates stats for _previous_ day only!
# NOTE: edit settings for your database below, This script generater stats only for configured vhost, not all of them! You need to setup separate scripts for every vhost.
#
error_reporting(E_ERROR);
print "\n[jorge] Generating stats.......";
$conn=mysql_connect("_YOUR_DATABASE_IP_", "_USERNAME_", "_PASSWORD_") or die ("DB connect failed\n");
mysql_select_db ("_DB_NAME_") or die ("DB select failed\n");

// hosts configuration:
$xmpp_host="_YOUR_XMPP_HOST_"; # Replace dot with underscore f.e.: jabber.org -> jabber_org
$vhost="_VHOST_FOR_WHICH_STATS_ARE_MADE"; # dotted form ex: jabber.org

$day_stats_a = date("Y-n-j", strtotime ("-1 day")); // if you mist stats for some day change to: date("Y-n-d", strtotime ("-1 day", strtotime("_YOUR_MISSING_DAY_"))); and generate stats manualy.
$day_stats_b = $day_stats_a;

for ($ds=0;$ds<24;$ds++) {

	$de=$ds+1;
	if ($de==24) {$de=0; $day_stats_b = date("Y-n-d", strtotime ("+1 day",strtotime($day_stats_a))); }
	$hourly_t="select count(owner_id) from `logdb_messages_$day_stats_a"."_"."$xmpp_host` where timestamp > unix_timestamp('$day_stats_a $ds:00:00') and timestamp < unix_timestamp('$day_stats_b $de:00:00')";
	$result=mysql_query($hourly_t);
	$row=mysql_fetch_row($result);
	$stats_insert="insert into jorge_stats (day,hour,value,vhost) values('$day_stats_a','$ds','$row[0]','$vhost')";
	mysql_query($stats_insert) or die("SQL Error\n");

}

print "done\n";



?>
