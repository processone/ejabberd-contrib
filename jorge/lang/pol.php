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

$vhost_select[pol] = "Wybierz serwer";
$vhost_not_selected[pol] = "Nie wybrano żadnego serwera!";
$no_script[pol] = "Twoja przeglądarka ma wyłączoną obsługę JavaScript. Jorge wymaga aby obsługa Javascript-u była włączona!";
$act_su[pol] = "Zapisywanie rozmów na serwerze zostało włączone!";
$wrong_data[pol] = "Nieprawidłowa nazwa użytkownika lub hasło!";
$wrong_data2[pol] = "Nieprawidłowe słowo z obrazka, spróbuj jeszcze raz";
$act_su2[pol] = "Aby przeglądać archiwa musisz zalogować się ponownie do systemu";
$act_info[pol] = "Usługa archiwizowania rozmów nie została aktywowana dla użytkownika: ";
$warning1[pol] = "System wykrył że nie posiadasz jeszcze profilu, który wymagany jest do pracy. Aktywuj swój profil teraz.<br>
			UWAGA: System jest w trakcie testów co oznacza że może nie działac w ogóle, działać wadliwie lub narazić Cię na utratę danych. Używasz go na własną odpowiedzialność!";
$welcome_1[pol] = "Jorge - archiwa rozmów. Zaloguj się do systemu";
$login_w[pol] = "Login";
$passwd_w[pol] = "Hasło";
$login_act[pol] = "Zaloguj się";
$devel_info[pol] = "Wersja BETA";
$activate_m[pol] = "AKTYWUJ";
$ch_lan[pol] = "Zmień język na:";
$ch_lan2[pol] = "Change language to ";
$lang_sw[pol] = "English";
$lang_sw2[pol] = "English";
$header_l[pol] = "Archiwa rozmów serwera";
$menu_item_browser[pol] = "Przeglądarka";
$menu_item_map[pol] = "Mapa Rozmów";
$menu_item_fav[pol] = "Ulubione";
$menu_item_search[pol] = "Wyszukiwarka";
$menu_item_links[pol] = "MyLinks";
$menu_item_panel[pol] = "Panel Sterowania";
$menu_item_contacts[pol] = "Kontakty";
$menu_item_logs[pol] = "Logi";
$menu_item_trash[pol] = "Kosz";
$filter_form[pol] = "Filtruj listę kontaktów<span style=\"vertical-align: super;\"><small> *</small></span>";
$filter_form_tip[pol] = "Wpisz nazwę kontaktu";
$filter_tip[pol] = "...a następnie wybierz z listy, lub przeszukaj listę ręcznie:";
$ff_notice[pol] = "Ta opcja działa tylko w przeglądarce Firefox";
$search_box[pol] = "Szukaj w archiwach";
$search_tip[pol] = "Wyświetlam";
$search_why[pol] = " wyników (<i>nie więcej niż 100</i>). <a href=\"help.php#22\" target=\"_blank\"><u>Dowiedz się dlaczego</u></a>";
$search_warn[pol] = "Uwaga: Wyszukuje tylko w wybranym przedziale czasu";
$all_for_u[pol] = "Pokaż wszystkie rozmowy używając: ";
$all_for_u_m[pol] = "strumienia";
$all_for_u_m_d[pol] = "Pokaż wszystkie rozmowy z tą osobą jako strumień wiadomości";
$all_for_u_m2[pol] = "mapy";
$all_for_u_m2_d[pol] = "Pokaż wszystkie rozmowy z tą osobą jako mapę rozmów";
$all_for_u_t[pol] = "Pokaż wszystkie rozmowy z tym użytkownikiem";
$arch_on[pol] = "Włącz archiwizacje";
$arch_off[pol] = "Wyłącz archiwizacje";
$log_out_b[pol] = "Wyloguj";
$archives_t[pol] = "Przeglądarka archiwum";
$main_date[pol] = "Data:";
$talks[pol] = "Lista rozmów:";
$thread[pol] = "Treść:";
$time_t[pol] = "Czas:";
$user_t[pol] = "Użytkownik:";
$my_links_save[pol] = "MyLinks";
$my_links_desc_m[pol] = "MyLinks - Twoje linki";
$my_links_desc_e[pol] = "Tutaj znajdziesz listę zapisanych fragmentów rozmów";
$settings_desc[pol] = "Ustawienia archiwum";
$settings_desc_detail[pol] = "Panel zawiera opcje pozwalające kontrolować archiwizacje rozmów oraz opcje dotyczące konta";
$api_access_enable[pol] = "Włącz publiczne API dla tego konta";
$api_access_disable[pol] = "Wyłącz publiczne API dla tego konta";
$api_access_on[pol] = "API dla tego konta jest włączone";
$api_access_learn[pol] = "Dowiedz się więcej na temat publicznego API";
$print_t[pol] = "drukuj";
$del_t[pol] = "usuń";
$resource_only[pol] = "Pokaż rozmowę tylko z tym zasobem";
$resource_warn[pol] = "Pokazuję rozmowę z zasobem: ";
$resource_discard[pol] = "Pokaż ";
$resource_discard2[pol] = "całą rozmowę.";
$del_all_conf[pol] = "Czy napewno chcesz usunąć *CAŁE* swoje archiwum wiadomości?\\nUWAGA: Nie będzie możliwości przywrócenia archiwum!";
$deleted_all[pol] = "Całe Twoje archiwum zostało usunięte";
$delete_nothing[pol] = "Twoje archiwum jest puste. Nic nie usunięto";
$delete_error[pol] = "Ooops...Wystąpiły błędy podczas wykonywania polecenia. Proszę spróbować poźniej";
$search_w1[pol] = "Wyszukiwany ciąg nie może być krótszy niż 3 i dłuższy niż 70 znaków...";
$search_res[pol] = "Wyniki wyszukiwania: ";
$my_links_save_d[pol] = "Zapisuje link. Wprowadź dane";
$my_links_optional[pol] = "Opis (opcjonalne, max 120 znakow)";
$my_links_chat[pol] = "Rozmowa z:";
$my_links_commit[pol] = "zapisz";
$my_links_cancel[pol] = "anuluj";
$my_links_link[pol] = "Link z dnia:";
$my_links_desc[pol] = "Opis:";
$my_links_added[pol] = "Link został zapisany!";
$my_links_back[pol] = "Wróć do rozmowy";
$my_links_removed[pol] = "Link został usunięty z bazy danych";
$my_links_none[pol] = "Brak opisu";
$status_msg1[pol] = "Archiwizacja rozmów jest aktualnie wyłączona";
$status_msg2[pol] = "Archiwizacja została włączona. (zmiany w profilu widoczne są po 10 sekundach)";
$status_msg3[pol] = "Archiwizacja została wyłączona. (zmiany w profilu widoczne są po 10 sekundach)";
$my_links_no_links[pol] = "Nie masz aktualnie zapisanych linków...";
$quest1[pol] = "Znalazłeś błąd? Zgłoś go!";
$search1[pol] = "Szukaj...";
$no_result[pol] = "Brak rezultatów wyszukiwania";
$settings_del[pol] = "Usuń całe archiwum";
$del_conf[pol] = "Czy na pewno usunąć tą rozmowę?";
$del_conf_my_link[pol] = "Czy na pewno usunąć ten link?";
$not_in_r[pol] = "Kontakt specjalny";
$del_moved[pol] = "Rozmowa została przeniesiona do kosza.";
$del_info[pol] = "Rozmowa została usunięta";
$undo_info[pol] = "Rozmowa została przywrócona";
$del_my_link[pol] = "usuń";
$help_but[pol] = "Pomoc";
$tip_delete[pol] = "Usuń historię rozmowy z tego dnia";
$tip_export[pol] = "Eksportuj rozmowę do pliku tekstowego";
$customize1[pol] = "Dostosuj logowanie";
$from_u[pol] = "Od: ";
$to_u[pol] = "Do: ";
$search_next[pol] = "Następne wyniki...";
$search_prev[pol] = "Poprzednie wyniki...";
$change_pass[pol] = "Zmień hasło";
$no_contacts[pol] = "Brak kontaktów na liście";
$no_archives[pol] = "W tej chwili nie masz zapisanych żadnych rozmów";
$con_tab1[pol] = "Lp.";
$con_tab2[pol] = "Nazwa kontaktu";
$con_tab3[pol] = "JabberID";
$con_tab4[pol] = "Włączyć archiwizacje";
$con_tab_act_y[pol] = "Tak";
$con_tab_act_n[pol] = "Nie";
$con_tab_submit[pol] = "Zapisz zmiany";
$con_tab6[pol] = "Grupa";
$con_no_g[pol] = "Brak grupy";
$map_no_g[pol] = "brak grupy";
$con_head[pol] = "Zarządzanie kontaktami";
$con_notice[pol] = "Uwaga: wyświetlane są tylko kontakty z przypisaną nazwą kontaktu.";
$con_title[pol] = "Kliknij na kontakcie aby zobaczyć archiwum rozmów";
$con_saved[pol] = "Zmiany zostały zapisane";
$help_notice[pol] = "Główne zagadnienia";
$nx_dy[pol] = "Kolejny dzień";
$no_more[pol] = "Brak większej ilości wyników";
$in_min[pol] = "minut";
$verb_h[pol] = "przerwa w rozmowie trwająca ponad godzinę";
$time_range_w[pol] = "Pole \"Od\" nie może być większe od pola \"Do\"";
$time_range_from[pol] = "od";
$time_range_to[pol] = "do";
$export_link[pol] = "eksportuj";
$export_head1[pol] = "Historia rozmowy między Tobą a ";
$export_head2[pol] = "przeprowadzona w dniu";
$help_search_tips[pol] = "
<br/><br/>
<li>Wyszukiwarka: Podpowiedzi.</li>
<ul>Przeszukując archiwa można zadawać kilka rodzajów zapytań na przykład:<br />
	żeby znaleźć wszystkie rozmowy z danym użytkownikiem wpisujemy w oknie wyszukiwania:<br />
	<b>from:jid@przykład.pl</b> - gdzie <i>jid</i> to nazwa użytkownika, a <i>przykład.pl</i> to serwer na którym wyszukiwana osoba ma konto.<br >
	aby wyszukać daną frazę w rozmowie z użytkownikiem możemy wykonać następujące zapytanie:<br />
	<b>from:jid@przykład.pl:co to jest jabber</b> - takie zapytanie przeszuka wszystkie rozmowy z użytkownikem <i>jid</i> z serwera <i>przykład.pl</i> w poszukiwaniu frazy: <i>co to jest jabber</i><br />
	Wyszukiwarka obsługuje oczywiście zwykłe wyszukiwanie - we wszystkich przeprowadzonych przez nas rozmowach:<br />
	<b>co to jest jabber</b> - wyszuka we wszystkich rozmowach frazy \"co to jest jabber\" jak również wyświetli wszystkie linie rozmowy zawierające słowa kluczowe<br />
	Jeśli nie znamy pełnej nazwy której poszukijemy możemy daną/dane litery zastąpić znakiem: * (gwiazdka) np.:<br />
	<b>jak*</b> - znajdzie wszystkie słowa zaczynające się na <i>jak</i> czyli np. <i>jaki, jaka</i>


</ul>

";
$help_my_links_note[pol] = "
<br/><br/>
<li>MyLinks: informacje ogólne.</li>
<ul>MyLinks służy do przechowywania(zapamiętywania) ulubionych fragmentów rozmów. Dzieki opcji MyLinks można w łatwy i szybki sposób odnaleźć poszukiwaną rozmowę.<br />
Aby dodać daną rozmowę do MyLinks należy kliknąć po prawej stronie okna z wyszukiwaną rozmową na opcji \"zapisz w mylinks\". Po wprowadzeniu opisu, link zostanie<br />
zapisany w zakładce MyLinks.
</ul>

";
$help_advanced_tips[pol] = "
<br/><br/>
<li>Jak szukać dokładnie?</li>
<ul>Wyszukiwarga <b>Jorge</b> obsługuje zaawansowane tryby wyszukiwania tzw. <i>Boolean mode</i>, co oznacza że znacznie można poprawić rezultaty wyszukiwania.<br/>
	Wyszukiwarka przeszukuje wszystkie Twoje archiwa w poszukiwaniu danej frazy, następnie ocenia tzw. <i>\"score\"</i>, sortuje dane i wyświetla najlepiej pasujące 100 wyników<br/>
	Aby ułatwić wyszukiwanie możesz użyć następujących modyfikatorów:<br>
	<b>+</b> - oznacza że dane słowo musi znaleźć się w wynikach wyszukiwania np. (+abc +def - odszuka wszystkie rozmowy zawierające w danej lini abc oraz def)<br>
	<b>-</b> - oznacza że dane słowo ma nie występować w wynikach wyszukiwania<br/>
	<b>></b> oraz <b><</b> - nadaje dodatkowe punkty wyszukiwanemu słowu w frazie. Np. poszukując linka wiemy że zawieta http i np. słowo planeta. Aby zwiększyć trafność wyników zapytanie powinno wyglądać tak: \"http &lt;planeta\"</br>
	<b>( )</b> - oznacza wykonanie pod-zapytania</br>
	<b>~</b> - dodaje negatywne punkty do danego słowa - ale go nie wyklucza z wyników</br>
	<b>*</b> - zastępuje ciąg znaków</br>
	<b>\"</b> - oznacza wyszukiwanie dokładnie pasującej frazy np: \"jak to\" znajdzie tylko rozmowy z dokładnie tą frazą

</ul>

";
$admin_site_gen[pol] = "Strona została wygenerowana w: ";
$logger_from_day[pol] = " z dnia: ";
$logger_overview[pol] = "Logi aktywności w Jorge";
$logger_f1[pol] = "Zdarzenie:";
$logger_f2[pol] = "Data zdarzenia:";
$logger_f3[pol] = "Poziom zdarzenia:";
$logger_f4[pol] = "Dodatkowe informacje:";
$logger_f_ip[pol] = "z adresu IP: ";
$refresh[pol] = "Odśwież";
$back_t[pol] = "Wróć na góre strony";
$trash_name[pol] = "Kosz";
$trash_desc[pol] = "Lista rozmów usuniętych. Wiadomości które przebywają w koszu dłużej niż 30 dni są automatycznie usuwane";
$trash_undel[pol] = "Przywróć";
$trash_vit[pol] = "Zobacz przywróconą rozmowę";
$trash_del[pol] = "Usuń";
$trash_link[pol] = "Akcja";
$trash_empty[pol] = "Kosz jest pusty";
$trash_recovered[pol] = "Rozmowa została przeniesiona do archiwum";
$cal_head[pol] = "Kalendarz rozmów.";
$cal_notice[pol] = "Kliknij na danym dniu aby zobaczyć rozmowy";
$change_view[pol] = "Zmień na widok drzewa";
$change_view_cal[pol] = "Przeglądaj archiwum za pomocą widoku kalendarza.";
$months_names = array("Styczeń","Luty","Marzec","Kwiecień","Maj","Czerwiec","Lipiec","Sierpień","Wrzesień","Październik","Listopad","Grudzień");
$weekdays = array("Poniedziałek","Wtorek","Środa","Czwartek","Piątek","Sobota","Niedziela");
$jump_to_l[pol] = "Wybierz miesiąc";
$chat_list_l[pol] = "Lista rozmów:";
$select_view[pol] = "Wybierz rodzaj widoku przeglądarki:";
$view_calendar[pol] = "Widok kalendarza";
$view_standard[pol] = "Widok drzewa";
$setting_d1[pol] = "Zmień globalną opcję archiwizacji:";
$setting_d2[pol] = "Usuń całe archiwum wiadomości (<i>nie można wycofać</i>):";
$chat_map[pol] = "Mapa rozmów";
$chat_select[pol] = "Wybierz kontakt aby zobaczyć listę rozmów";
$chat_m_select[pol] = "Wybierz kontakt:";
$chat_c_list[pol] = "Lista kontaktów";
$chat_no_chats[pol] = "Brak rozmów z wybranym kontaktem";
$chat_map_back[pol] = "<<< Wróć do Mapy Rozmów";
$fav_back[pol] = "<<< Wróć do Ulubionych";
$myl_back[pol] = "<<< Wróć do MyLinks";
$sel_language[pol] = "Wybierz preferowany język";
$sel_client[pol] = "Uruchom Slimster";
$sel_yes[pol] = "Tak";
$sel_no[pol] = "Nie";
$jump_to_next[pol] = "Przejdź do następnego dnia rozmowy";
$jump_to_prev[pol] = "Przejdź do poprzedniego dnia rozmowy";
$show_chats[pol] = "Pokaż rozmowę jako";
$show_chat_stream[pol] = "strumień";
$show_chat_as_map[pol] = "mapę";
$tip_next_m[pol] = "Przejdź do następnego miesiąca";
$tip_prev_m[pol] = "Przejdź do poprzedniego miesiąca";
$cal_days[pol]['1'] = "Pon";
$cal_days[pol]['2'] = "Wto";
$cal_days[pol]['3'] = "Śro";
$cal_days[pol]['4'] = "Czw";
$cal_days[pol]['5'] = "Pią";
$cal_days[pol]['6'] = "Sob";
$cal_days[pol]['7'] = "Nie";
$chat_lines[pol] = "Ilość wiadomości: ";
$del_time[pol] = "Usunięto:";
$marked_as_d[pol] = "Ta rozmowa znajduje się w koszu. Aby ją przeglądać musisz ją <a href=\"trash.php\"><u>przywrócić</u></a>";
$stats_personal_d[pol] = "Statystyki rozmów";
$stats_personal[pol] = "Twoja całkowita liczba wiadomości w archiwum:";
$stats_personal_top[pol] = "10 najdłuższych rozmów:";
$stats_when[pol] = "Kiedy";
$stats_personal_count[pol] = "Liczba wiadomości";
$stats_peer[pol] = "Rozmówca";
$stats_see[pol] = "Zobacz rozmowę";
$stats_for[pol] = "Statystyki dla: ";
$stats_messages[pol] = "Serwer zalogował ";
$stats_messages_b[pol] = " wiadomości w ";
$stats_messages_c[pol] = " rozmowach.";
$stats_graph1[pol] = "Całkowita liczba użytkowników korzystających z archiwizacji (dziennie)";
$stats_graph2[pol] = "Ilość wiadomości zalogowanych przez serwer (dziennie)";
$stats_graph3[pol] = "Zalogowane wiadomości (godzinowo)";
$stats_graph4[pol] = "Zalogowane wiadomości (tygodniowo)";
$stats_top[pol] = "Najdłuższe rozmowy z ostatnich dni:";
$stats_not_eno[pol] = "Brak wystarczających danych do narysowania statystyk (<i>minimum 30 dni</i>)";
$fav_main[pol] = "Ulubione";
$fav_desc[pol] = "Lista rozmów oznaczonych jako \"Ulubione\"";
$fav_add[pol] = "Dodaj rozmowę do ulubionych";
$fav_chat[pol] = "Rozmowa z: ";
$fav_success[pol] = "Rozmowa została dodana do Twoich <i>Ulubionych</i> !";
$fav_discard[pol] = "Ukryj tą informacje";
$fav_exist[pol] = "Ooops...Ta rozmowa juz znajduje się w Twoich <i>Ulubionych</i>";
$fav_favorited[pol] = "Ta rozmowa jest dodana do ulubionych";
$fav_contact[pol] = "Rozmowa z:";
$fav_when[pol] = "Kiedy:";
$fav_comment[pol] = "Komentarz:";
$fav_nocomm[pol] = "Brak komentarza";
$fav_add_comment[pol] = "Dodaj komentarz";
$fav_remove[pol] = "usuń";
$fav_removed[pol] = "Rozmowa została usnięta z <i>Ulubionych</i>";
$fav_empty[pol] = "Nie masz aktualnie zapisanych żadnych <i>Ulubionych</i> rozmów";
$fav_error[pol] = "Oooups...Wystąpił błąd podczas dodawania rozmowy";
$reset_sort[pol] = "resetuj sortowanie";
$cont_chat[pol] = "rozmowa kontynuowana jest następnego dnia >>>";
$cont_chat_p[pol] = "<<< rozmowa jest kontynuacją z dnia poprzedniego";
$close_account[pol] = "Usuń konto z serwera:";
$close_info[pol] = "UWAGA: wraz z kontem XMPP zostanie usunięte konto z Google Apps!";
$close_warn[pol] = "Czy napewno usunąć konto i wszystkie wiadomości?";
$close_commit[pol] = "- Usuń teraz -";
$close_failed[pol] = "Usunięcie konta nie powiodło się. Proszę spróbować później";
$oper_fail[pol] = "<center><b>Operacja nie została wykonana! Proszę spróbować później lub skontaktować się z administratorem!</b></center>";
$go_to_jorge[pol] = "Idz do strony glownej";
$qlink_l[pol] = "Przejdź do najnowszych rozmów";
$message_type_message[pol] = "Wiadomość";
$message_type_error[pol] = "Wiadomość została oznaczona jako zawierająca błąd i prawdopodobnie nie została dostarczona.";
$message_type_headline[pol] = "Headline";
$muc_message[pol] = "Wiadomość systemowa:";
$spec_contact_enable[pol] = "Wyświetlać kontakty specjalne:";
$spec_contact_desc[pol] = "Pozwala zdecydować czy w liście rozmów mają pojawiać się rozmowy z kontaktami specjalnymi np.: bramkami lub transportami. Większość użytkowników może spokojnie tą opcje wyłączyć";
$donate[pol] = "<small>Pomóż rozwijać Projekt Jorge.<a href=\"http://www.jabster.pl/index.php?q=node/43\" target=\"_blank\"><u>Przeczytaj więcej...</u></a></small>";
$donate_dont[pol] = "<small>Nie pokazuj tej informacji więcej...</small>";
$own_name_desc[pol] = "Opcja ta umożliwia zmianę wyświetlanej nazwy użytkownika: np. <i>karol002a</i> na <i>Karol</i>";
$own_name_enter[pol] = "Podaj swoją nazwę:";
$own_name_commit[pol] = "Ustaw nazwę";
$own_name_remove[pol] = "Jeśli nie chcesz korzystać z tej opcji, pozostaw pole puste";
$stats_vhost_select[pol] = "Wybierz serwer dla którego chcesz oglądać statystyki: ";

?>
