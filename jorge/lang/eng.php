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

// WARNING: File encoding is UTF-8 and should remain in this encoding!

if (__FILE__==$_SERVER['SCRIPT_FILENAME']) {

	header("Location: index.php?act=logout");
	exit;

}

$vhost_select[eng] = "Select server";
$vhost_not_selected[eng] = "No server have been selected!";
$no_script[eng] = "Your browser dont support Javascript. Jorge require to have Javascript enabled!";
$act_su[eng] = "Message archiving activated succesfuly!";
$wrong_data[eng] = "Bad username or password!";
$wrong_data2[eng] = "You have entered wrong word from picture, try again";
$act_su2[eng] = "You must relogin to system";
$act_info[eng] = "Message archiving is not activated for user: ";
$warning1[eng] = "System discovered that you dont have profile yet, profile is required to work with Jorge. Create your profile now.<br>
			WARNING - We are still testing the system, that mean it may not work at all or work wrong or even lead to datalost. Use it at your own risk!";
$welcome_1[eng] = "Welcome to Jorge - message archives. Please login";
$login_w[eng] = "Login";
$passwd_w[eng] = "Password";
$login_act[eng] = "Sign in";
$devel_info[eng] = "Development version";
$activate_m[eng] = "Enable message archiving";
$ch_lan[eng] = "Change language to:";
$ch_lan2[eng] = "Zmień język na ";
$lang_sw[eng] = "Polski";
$lang_sw2[eng] = "Angielski";
$header_l[eng] = "Message archives of server";
$menu_item_browser[eng] = "Browser";
$menu_item_map[eng] = "Chat Map";
$menu_item_fav[eng] = "Favorites";
$menu_item_search[eng] = "Search";
$menu_item_links[eng] = "MyLinks";
$menu_item_panel[eng] = "Control Panel";
$menu_item_contacts[eng] = "Contacts";
$menu_item_logs[eng] = "Logs";
$menu_item_trash[eng] = "Trash";
$filter_form[eng] = "Filter your contacts<span style=\"vertical-align: super;\"><small> *</small></span>";
$filter_form_tip[eng] = "Type contact name";
$filter_tip[eng] = "...and next select it from list, or search contact list by hand:";
$ff_notice[eng] = "This function work only in Firefox browser";
$search_box[eng] = "Search in archives";
$search_tip[eng] = "Displaying";
$search_why[eng] = " search results (<i>not more then 100</i>). <a href=\"help.php#22\" target=\"_blank\"><u>Find out why</u></a>";
$search_warn[eng] = "Warning: Showing results only from selected time range";
$all_for_u[eng] = "Show conversations with this user using: ";
$all_for_u_m[eng] = "stream";
$all_for_u_m_d[eng] = "Show all conversations with this user as stream";
$all_for_u_m2[eng] = "map";
$all_for_u_m2_d[eng] = "Show all conversations with this user as chat map";
$all_for_u_t[eng] = "Show all chats from this user";
$arch_on[eng] = "Turn on archivization";
$arch_off[eng] = "Turn off archivization";
$log_out_b[eng] = "Sign out";
$archives_t[eng] = "Archive browser";
$main_date[eng] = "Date:";
$talks[eng] = "Conversation list:";
$thread[eng] = "Content:";
$time_t[eng] = "Time:";
$user_t[eng] = "User:";
$my_links_save[eng] = "MyLinks";
$my_links_desc_m[eng] = "MyLinks - Your links";
$my_links_desc_e[eng] = "Here you can find saved fragments of your chats";
$settings_desc[eng] = "Archive settings";
$settings_desc_detail[eng] = "The panel consist of options that let you control message archiving as well as options regarding your account";
$api_access_enable[eng] = "Enable API access for this account";
$api_access_disable[eng] = "Disable API access for this account";
$api_access_off[eng] = "API access is enabled for this account";
$api_access_learn[eng] = "Learn more about public API";
$print_t[eng] = "print";
$del_t[eng] = "delete";
$resource_only[eng] = "Show chat only with this resource";
$resource_warn[eng] = "Showing chat only with resource: ";
$resource_discard[eng] = "Show ";
$resource_discard2[eng] = "entire chat thread.";
$del_all_conf[eng] = "You are about to delete all your message archives. Are you *really* sure?\\nWARNING: It would be impossible to recover your archives!";
$deleted_all[eng] = "All your message archive has been deleted";
$delete_nothing[eng] = "Your message archive is empty. Nothing was deleted";
$delete_error[eng] = "Ooops...There were errors during processing your request. Please try again later";
$search_w1[eng] = "Search string cannot be shorter than 3 and longer than 70 characters...";
$search_res[eng] = "Search results: ";
$my_links_save_d[eng] = "Saving link. Fill the form below";
$my_links_optional[eng] = "Description (optional, max 120 characters)";
$my_links_chat[eng] = "Conversation with:";
$my_links_commit[eng] = "save";
$my_links_cancel[eng] = "cancel";
$my_links_link[eng] = "Link from day:";
$my_links_desc[eng] = "Description:";
$my_links_added[eng] = "Link succesfuly added!";
$my_links_back[eng] = "Back to chat";
$my_links_removed[eng] = "Link succesfuly deleted";
$my_links_none[eng] = "No decsription";
$status_msg1[eng] = "Message archiving is disabled by user";
$status_msg2[eng] = "Message archiving have beed enabled. Changes may take 10s";
$status_msg3[eng] = "Message archiving have beed disabled. Changes may take 10s";
$my_links_no_links[eng] = "You don't have any MyLinks saved...";
$quest1[eng] = "Found error? Fill bug report!";
$search1[eng] = "Search...";
$no_result[eng] = "No search results";
$settings_del[eng] = "Delete entire archive";
$del_conf[eng] = "Do you really want to delete this chat?";
$del_conf_my_link[eng] = "Do you really want to remove that link?";
$not_in_r[eng] = "Special contact";
$del_moved[eng] = "Conversation moved to trash.";
$del_info[eng] = "Conversation have been deleted";
$undo_info[eng] = "Conversation restored succesfuly";
$del_my_link[eng] = "delete";
$help_but[eng] = "Help";
$tip_delete[eng] = "Delete this conversation";
$tip_export[eng] = "Export this conversation to text file";
$customize1[eng] = "Customize logging";
$from_u[eng] = "From: ";
$to_u[eng] = "To: ";
$search_next[eng] = "Next results...";
$search_prev[eng] = "Previous results...";
$change_pass[eng] = "Change password";
$no_contacts[eng] = "Your contacts list is currently empty";
$no_archives[eng] = "Currently you dont have any chats saved";
$con_tab1[eng] = "No.";
$con_tab2[eng] = "Contact name";
$con_tab3[eng] = "JabberID";
$con_tab4[eng] = "Enable archiving";
$con_tab_act_y[eng] = "Yes";
$con_tab_act_n[eng] = "No";
$con_tab_submit[eng] = "Save changes";
$con_tab6[eng] = "Group";
$con_no_g[eng] = "No group";
$map_no_g[eng] = "no group";
$con_head[eng] = "Contacts managment";
$con_notice[eng] = "Notice: displaying only contacts with assigned nicknames.";
$con_title[eng] = "Click on contact name to see conversation history";
$con_saved[eng] = "Changes have beed saved";
$help_notice[eng] = "Main topics";
$nx_dy[eng] = "Next day";
$no_more[eng] = "No more search results";
$in_min[eng] = "minutes";
$verb_h[eng] = "interruption in conversation lasting more than an hour";
$time_range_w[eng] = "Field \"From\" cannot be greater than field \"To\"";
$time_range_from[eng] = "from";
$time_range_to[eng] = "to";
$export_link[eng] = "export";
$export_head1[eng] = "Exported chat between you and ";
$export_head2[eng] = "performed on";
$help_search_tips[eng] ="
<br/><br/>
<li>Search Tips</li>
<ul>When searching you can do some more complex queries like:<br />
	if you want to find all chats from particular user you can type:<br />
	<b>from:jid@example.com</b> - where <i>jid</i> is user name of the server: <i>example.com</i><br />
	or if you want to find phase in chats with that user, you can type:<br />
	<b>from:jid@example.com:what is jabber</b> - witch will query for phase <i>what is jabber</i> in all chats with user <i>jid@example.com</i><br />
	Search engine also of course supports normal search that search all archives:<br />
	<b>what is jabber</b> - will search in all our chats phase \"what is jabber\" as well as all keywords like: \"what\", \"is\", \"jabber\"<br />
	If we don't know full name that we are searching we can put instead character: * (wildcard):<br />
	<b>wor*</b> - will find all words that begin with wor* like: word, work, world...
</ul>
";
$help_my_links_note[eng] = "
<br/><br/>
<li>MyLinks: overview.</li>
<ul>MyLinks let you store your favorited links. Thanks to MyLinks option you can easly and fast find your favorited talk.<br />
To add chat to MyLinks just click on the right side of the chat window onto option called \"save in mylinks\". Then fill the form with description and save link into database.
</ul>




";
$help_advanced_tips[eng] = "
<br/><br/>
<li>How to search right</li>
<ul>Search engine of <b>Jorge</b> supports advanced mode called <i>Boolean mode</i>, that means that you can improve your search results.</br>
	Search engine search all your archives next it sort it and evaluates score and then displays only 100 most relevant matches.<br/>
	To let you make it easy to adjust search results engine supports following arguments:<br/>
	<b>+</b> - means that particular word must be in the results, so: +abc +def means that both words must be there<br/>
	<b>-</b> - it excludes word from search results<br/>
	<b>></b> and <b><</b> - increasese or decreases score for particular word</br>
	<b>( )</b> - make it possible to execute sub-query</br>
	<b>~</b> - adds negative score to particular word</br>
	<b>*</b> - replaces unknown word</br>
	<b>\"</b> - perform exact match search</br>
</ul>


";
$admin_site_gen[eng] = "Site generated in:";
$logger_from_day[eng] = " from day: ";
$logger_overview[eng] = "Activity logs on Jorge";
$logger_f1[eng] = "Event:";
$logger_f2[eng] = "Event date:";
$logger_f3[eng] = "Event level:";
$logger_f4[eng] = "Additional info:";
$logger_f_ip[eng] = "from IP address: ";
$refresh[eng] = "Refresh";
$back_t[eng] = "Back to top of the page";
$trash_name[eng] = "Trash";
$trash_desc[eng] = "List of trashed conversations. Conversations that are left in trash are automaticly deleted after 30 days.";
$trash_undel[eng] = "Restore";
$trash_vit[eng] = "View restored chat";
$trash_del[eng] = "Delete";
$trash_link[eng] = "Action";
$trash_empty[eng] = "Trash is empty";
$trash_recovered[eng] = "Conversation have been moved to archive";
$cal_head[eng] = "Chat calendar";
$cal_notice[eng] = "Click on days to see chats";
$change_view[eng] = "Switch to tree view";
$change_view_cal[eng] = "Browse archives using calendar view.";
$months_names = array("January","February","March","April","May","June","July","August","September","October","November","December");
$weekdays = array("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday");
$jump_to_l[eng] = "Select month";
$chat_list_l[eng] = "Conversation list:";
$select_view[eng] = "Select prefered view for browser:";
$view_calendar[eng] = "Calendar view";
$view_standard[eng] = "Tree view";
$setting_d1[eng] = "Change global archivization policy:";
$setting_d2[eng] = "Delete entire message archive (<i>cannot undo</i>):";
$chat_map[eng] = "Chat map";
$chat_select[eng] = "Select contact to see chats";
$chat_m_select[eng] = "Pick a contact:";
$chat_c_list[eng] = "Contacts list";
$chat_no_chats[eng] = "There are no chats with selected contact";
$chat_map_back[eng] = "<<< Back to ChatMap";
$fav_back[eng] = "<<< Back to Favorites";
$myl_back[eng] = "<<< Back to MyLinks";
$sel_language[eng] = "Select prefered language";
$sel_client[eng] = "Launch Slimster";
$sel_yes[eng] = "Yes";
$sel_no[eng] = "No";
$jump_to_next[eng] = "Go to next conversation with this user";
$jump_to_prev[eng] = "Go to to previous conversation with this user";
$show_chats[eng] = "Show chat as";
$show_chat_stream[eng] = "stream";
$show_chat_as_map[eng] = "map";
$tip_next_m[eng] = "Go to next month";
$tip_prev_m[eng] = "Go to previous month";
$cal_days[eng]['1'] = "Mon";
$cal_days[eng]['2'] = "Tue";
$cal_days[eng]['3'] = "Wed";
$cal_days[eng]['4'] = "Thu";
$cal_days[eng]['5'] = "Fri";
$cal_days[eng]['6'] = "Sat";
$cal_days[eng]['7'] = "Sun";
$chat_lines[eng] = "Messages count: ";
$del_time[eng] = "Deleted:";
$marked_as_d[eng] = "This chat is in trash. If you want to see it - <a href=\"trash.php\"><u>restore it</u></a>";
$stats_personal_d[eng] = "Personal statistics";
$stats_personal[eng] = "Your total number of messages in archiwe:";
$stats_personal_top[eng] = "Your top 10 chats:";
$stats_when[eng] = "When";
$stats_personal_count[eng] = "Messages count";
$stats_peer[eng] = "Talker";
$stats_see[eng] = "See this chat";
$stats_for[eng] = "Stats for: ";
$stats_messages[eng] = "Messages logged by server: ";
$stats_messages_b[eng] = " in ";
$stats_messages_c[eng] = " conversations.";
$stats_graph1[eng] = "Total number of users using message archiving (daily)";
$stats_graph2[eng] = "Messages logged by server (daily)";
$stats_graph3[eng] = "Messages logged (hourly)";
$stats_graph4[eng] = "Messages logged (weekly)";
$stats_top[eng] = "Longest conversations from last days:";
$stats_not_eno[eng] = "Not enought data to plot the graphs (<i>30 days needed</i>)";
$fav_main[eng] = "Favorites";
$fav_desc[eng] = "List of chats marked as favorites";
$fav_add[eng] = "Add conversation to favorites";
$fav_chat[eng] = "Conversation with: ";
$fav_success[eng] = "Conversation has been succesfully added to your <i>Favorites</i> !";
$fav_discard[eng] = "Discard this message";
$fav_exist[eng] = "Ooops...This chat is already in your <i>Favorites</i>";
$fav_favorited[eng] = "This conversation is already added to favorites";
$fav_contact[eng] = "Conversation with:";
$fav_when[eng] = "When:";
$fav_comment[eng] = "Comment:";
$fav_nocomm[eng] = "No comment";
$fav_add_comment[eng] = "Add comment";
$fav_remove[eng] = "delete";
$fav_removed[eng] = "Conversation has been deleted from <i>Favorites</i>";
$fav_empty[eng] = "You dont have any <i>Favorites</i> chats saved";
$fav_error[eng] = "Oooups...There was a problem during processing your request";
$reset_sort[eng] = "reset sorting";
$cont_chat[eng] = "chat continues on next day >>>";
$cont_chat_p[eng] = "<<< this chat is continuation from last day";
$close_account[eng] = "Close your account:";
$close_info[eng] = "WARNING: during account removal also account on Google Apps will be removed!";
$close_warn[eng] = "Do you really want to remove all messages and user account?";
$close_commit[eng] = "- Close now -";
$close_failed[eng] = "Close account failed. Please try again later";
$oper_fail[eng] = "<center><b>Operation failed! Please try again later or contact administrator!</b></center>";
$go_to_jorge[eng] = "Go to Jorge main page";
$qlink_l[eng] = "Go to latest conversations";
$message_type_message[eng] = "Message";
$message_type_error[eng] = "Message have been marked as faulty, and probably was not delivered.";
$message_type_headline[eng] = "Headline";
$muc_message[eng] = "System message:";
$spec_contact_enable[eng] = "Display special contact:";
$spec_contact_desc[eng] = "This option let you decide if conversations with special contacts like gateways, transports are displayed in your chat list. Most users can say <i>No</i> here.";
$donate[eng] = "<small>Help develop Project Jorge.<a href=\"http://www.planeta.toliman.pl/2008/10/donate-project-jorge.html\" target=\"_blank\"><u>Read More...</u></a></small>";
$donate_dont[eng] = "<small>Don't show this information anymore...</small>";
$own_name_desc[eng] = "This option allows to display your name as you wish: f.e. <i>james0021a</i> is replaced with <i>James</i>";
$own_name_enter[eng] = "Enter your name:";
$own_name_commit[eng] = "Set your name";
$own_name_remove[eng] = "If you dont want to use this option, leave it unchanged";
$stats_vhost_select[eng] = "Select server for viewing its statistics: ";

?>
