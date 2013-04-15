<?php
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

class session {

    public $id;

    function session ($lifetime=10800) { // czas ?ycia sesji
          @session_start();
    }

    function unregister($name) {
        unset($HTTP_SESSION_VARS[$name]);
    }

    function is_registered($name) {
        if (isset($_SESSION[$name])) return true;
          else return false;
    }

    function get($name) {
        return $_SESSION[$name];
    }

    function set($name,$value) {
        $_SESSION[$name]=$value;
    }

    // zwraca id sesji uzytkownika
    function id() {
          return(@session_id());
    }

    // ubija sesje - logout
    function finish() {
          //$id_session = $this->id();
          @session_unset();
          @session_destroy();
    }

}
?>
