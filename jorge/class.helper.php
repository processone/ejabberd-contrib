<?
/*
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

###########################################################################

Helper classes for Jorge. Performs various operations.


*/

Class url_crypt Extends parser {

	private $td;

	public function __construct($key) {
		
		$td = mcrypt_module_open('des', '', 'ecb', '');
		$key = substr($key, 0, mcrypt_enc_get_key_size($td));
		$iv_size = mcrypt_enc_get_iv_size($td);
		$iv = mcrypt_create_iv($iv_size, MCRYPT_RAND);
		mcrypt_generic_init($td, $key, $iv);
		$this->td = $td;

	}

	public function __destruct() {

		mcrypt_generic_deinit($this->td);
		mcrypt_module_close($this->td);

	}

	public function crypt_url($url) {

		return str_replace("+", "kezyt2s0", $this->url_encrypt($url));
	}

	public function decrypt_url($url) {

		$url = str_replace("kezyt2s0", "+",$url);
		return $this->decode_string($this->url_decrypt(base64_decode($url)));

	}

	private function url_encrypt($url) {

		$prepared_string = "begin&".$url;
		$integrity = md5($prepared_string);
		$url = "integrity=$integrity&".$prepared_string;
		$td = $this->td;
        	$c_t = mcrypt_generic($td, $url);
		return base64_encode($c_t);

	}

	private function url_decrypt($url) {

		$td = $this->td;
		$p_t = mdecrypt_generic($td, $url);
		return trim($p_t);

	}

}


Class parser {

	public $tslice = null;
	public $peer_name_id = null;
	public $peer_name = null;
	public $peer_server_id = null;
	public $peer_server = null;
	public $jid = null;
	public $ismylink = null;
	public $linktag = null;
	public $strt = null;
	public $lnk = null;
	public $action = null;
	public $search_phase = null;
	public $offset_arch = null;
	public $offset_day = null;
	public $tag_count = null;
	public $time_start = null;
	public $time_end = null;
	public $single = null;


	protected function decode_string($url) {

		parse_str($url);
		$reconstructed = strstr($url,"begin");
		settype($integrity,"string");
		if ($integrity === md5($reconstructed)) { 
				
				if (isset($tslice)) { 
						$this->tslice = $tslice; 
					}
				if (isset($peer_name_id)) { 
						$this->peer_name_id = $peer_name_id; 
					}
				if (isset($peer_server_id)) { 
						$this->peer_server_id = $peer_server_id; 
					}
				if (isset($jid)) {
						$this->jid = $jid;
					}
				if (isset($lnk)) { 
						$this->lnk = $lnk; 
					}
				if (isset($ismylink)) { 
						$this->ismylink = $ismylink; 
					}
				if (isset($linktag)) { 
						$this->linktag = $linktag; 
					}
				if (isset($strt)) { 
						$this->strt = $strt; 
					}
				if (isset($action)) { 
						$this->action = $action; 
					}
				if (isset($peer_name)) {
						$this->peer_name = $peer_name;
					}
				if (isset($peer_server)) {
						$this->peer_server = $peer_server;
					}
				if (isset($search_phase)) {
						$this->search_phase = $search_phase;
					}
				if (isset($offset_arch)) {
						$this->offset_arch = $offset_arch;
					}
				if (isset($offset_day)) {
						$this->offset_day = $offset_day;
					}
				if (isset($tag_count)) {
						$this->tag_count = $tag_count;
					}
				if (isset($time_start)) {
						$this->time_start = $time_start;
					}
				if (isset($time_end)) {
						$this->time_end = $time_end;
					}
				if (isset($single)) {
						$this->single = $single;
					}
				
				return true;
				
			} 
			else { 
				
				return false;
				
			}

	return false;
	}


}

Class render_html {

	protected $html_head = array();
	protected $html_menu = array();
	protected $html_over;
	protected $html_main = array();
	protected $html_body = array();
	protected $html_foot = array();
	private $head_items = integer;
	private $menu_items = integer;
	private $main_items = integer;
	private $body_items = integer;
	private $foot_itesm = integer;

        public function system_message($html) {

                $this->html_main = array("sys_message"=>$this->render_system($html));
                return;

        }

	public function status_message($html) {

		$this->html_main = array("status_message"=>$this->render_status($html));
		return;
	
	}

	public function alert_message($html) {

		$this->html_main = array("alert_message"=>$this->render_alert($html));
		return;
	
	}

	public function headers($html) {

		if ($this->head_items === 0) {

				$this->html_head = array("0"=>$html);
				$this->head_items = 1;

			}
			else{

				$this->head_items = $this->head_items + 1;
				$this->html_head = $this->html_head += array($this->head_items=>$html);

		}
                return;

        }

        public function menu($html) {

                if ($this->menu_items === 0) {

                                $this->html_menu = array("0"=>$html);
				$this->menu_items = 1;

                        }
                        else{

				$this->menu_items = $this->menu_items + 1;
				$this->html_menu = $this->html_menu += array($this->menu_items=>$html);
                }
                return;

        }

	public function set_overview($html) {

		$this->html_over = $html;
		return;

	}

        public function set_body($html) {

                if ($this->body_items === 0) {

                                $this->html_body = array("0"=>$html);
				$this->body_items = 1;

                        }
                        else{

				$this->body_items = $this->body_items + 1;
				$this->html_body = $this->html_body += array($this->body_items=>$html);
                }
                return;

        }

        public function foot($html) {

		if ($this->foot_items === 0) {
				
				$this->html_foot = array("0"=>$html);
				$this->foot_items = 1;
			}
			else{

				$this->foot_items = $this->foot_items + 1;
				$this->html_foot = $this->html_foot += array($this->foot_items=>$html);
		}
                return;

        }

        public function commit_render() {
		
		$html_head = $this->html_head;
		$html_menu = $this->html_menu;
		$html_over = $this->html_over;
                $html_main = $this->html_main;
                $html_body = $this->html_body;
		$html_foot = $this->html_foot;

		for ($z=0;$z<=$this->head_items;$z++) {
			
                	$out  .= $html_head[$z];

		}
		$out .= $html_main[sys_message];
		for ($z=0;$z<=$this->menu_items;$z++) {

			$out .= $html_menu[$z];
		
		}
		$out .= $html_over;
		$out .= $html_main[alert_message];
		$out .= $html_main[status_message];
                for ($z=0;$z<=$this->body_items;$z++) {

                        $out .= $html_body[$z];

                }
		for ($z=0;$z<=$this->foot_items;$z++) {

                	$out .= $html_foot[$z];

		}
                echo $out;
		return;

        }

	public function destroy_content() {

		$this->html_body = array();
		return;

	}

	protected function render_alert($message, $class = "message") {


		return '<center><div class="'.$class.'">'.$message.'</div><br></center>';

	}

	protected function render_status($message, $class = "message") {

		return '<center><div class="'.$class.'">'.$message.'</div><br></center>';
	
	}

	protected function render_system($message, $class = null) {

		return '<center><div class="system">'.$message.'</div><br></center>';

	}


}

?>
