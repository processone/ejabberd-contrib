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

$location=$_SERVER['PHP_SELF'];


if ($location=="footer.php") {
	header('Location: index.php?act=logout');
	exit;
}

if (!preg_match("/index.php/i",$location)) {

	$html->foot('<br><div align="right" style="clear: left;"><a href="http://dev.jabbim.cz/jorge/newticket" target="_blank">'.$quest1[$lang].'</a></div>');

}

$html->foot('
	<br><div style="background-image: url(img/bell-down.png); height: 3px; background-repeat:repeat-x;"></div>
	<div align="center">'.$links.'</div><br>
	<div align="center" style="color: gray;">'.$copy.'</div>
	<p style="font-size: xx-small; text-align: right;">v1.5-RC1</p><br>
	');

// footer for admins...
$time_end = getmicrotime();
$time = substr($time_end - $time_start, 0, 10);
if (TOKEN==ADMIN_NAME) { 

	$html->foot('<small>'.$admin_site_gen[$lang].$time.'s.</small>');
	
};

// execude following code only when user is logged in
if (!preg_match("/index.php/i",$location) AND !preg_match("/not_enabled.php/i",$location)) {

	$html->foot('

	<script type="text/javascript">
		$(document).ready(function() {
	        function format(mail) {
			return mail.name + " &lt;" + mail.jid + "&gt";
		}
		$("#t_search").autocomplete([');

	$ejabberd_roster->sort_by_jid("az");
	$roster_auto = $ejabberd_roster->get_roster();
	while(array_keys($roster_auto)) {

		$jid = htmlspecialchars(key($roster_auto));
		$nic = htmlspecialchars($roster_auto[$jid][nick]);
		array_shift($roster_auto);
		$html->foot('{ name: "'.$nic.'", jid:"'.$jid.'" },');

	}

	$html->foot('],
		{
		minChars: 1,
		parse: function(data) {
			return $.map(eval(data), function(row) {
				return {
					data: row,
					value: row.name,
					result: row.name + " <" + row.jid + ">"
				}
			});
		},
		formatItem:function(row, i, max, term) {
			return row.name.replace(new RegExp("(" + term + ")", "gi"), "<strong>$1</strong>") + "<br><span style=\'font-size: 80%;\'>JabberID: &lt;" + row.jid + "&gt;</span>";
		},
		formatResult: function(row) {
		                        return "from:" + row.jid;
		},
		multiple: false,
		max: 10,
		cacheLength: 200,
		matchSubset: true,
		selectFirst: true,
		matchContains: true
		}

			).result(function(e, item) {
				$("#content").append("<p>selected " + format(item) + "</p>");
			});
		});

	</script>

	');

}

$html->foot('</body></html>');

// render html output
$html->commit_render();

ob_end_flush();
?>
