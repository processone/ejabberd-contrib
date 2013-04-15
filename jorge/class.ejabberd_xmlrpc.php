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

/*##################################################

PHP XML-RPC clss for mod_xmlrpc
-------------------------------

Author: Zbyszek Zolkiewski (zbyszek@jabster.pl)
TEST VERSION, NOT ALL CALLS INCLUDED! THERE IS NO DOCUMENTATION YET.

$ejabberd_rpc = new rpc_connector("IP_OF_RPC_SERVER","RPC_PORT","XMPP_HOST","OPTIONAL_USERNAME","OPTIONAL_PASSWORD","OPTIONAL_NEWPASS");

if no username is provided on object creation, you can set it via:
$ejabberd_rpc->set_user("username","password","optional-new_password");

this is usefull especialy for scripts like bulk account creation/deletion etc...

Method:			Returned values:
crete_account() 	true|false|exist
delete_account() 	true|false
auth()			true|false
check_account()		true|flase
change_password()	true|false
get_roster()		Array()
test_rpc()		String

Example of authentication:

try {
	if($ejabberd_rpc->auth() === true) {

				print "Auth OK";
			}
			else {
				print "Unknown account or bad password";
			}
}
catch(Exception $e) {
        echo "Exception: ".$e->getMessage();
	echo ", Code: ".$e->getCode();
}

*/##################################################

class rpc_connector {

	protected $rpc_server;
	protected $rpc_port;
	protected $username;
	protected $password;
	protected $newpass;
	protected $vhost;
	protected $parms;
	protected $method;

	public function __construct($rpc_server,$rpc_port,$vhost,$username = null,$password = null,$newpass = null) {
		$this->setData($rpc_server,$rpc_port,$vhost,$username,$password,$newpass);
	}


	protected function setData($rpc_server, $rpc_port, $vhost, $username, $password,$newpass) {
		$this->rpc_server = $rpc_server;
		$this->rpc_port = $rpc_port;
		$this->vhost = $vhost;
		$this->username = $username;
		$this->password = $this->clean_password($password);
		$this->newpass = $this->clean_password($newpass);
	}

	public function set_user($username,$password,$newpass = null) {

		$this->username = $username;
		$this->password = $this->clean_password($password);
		$this->newpass = $this->clean_password($newpass);

	}

	protected function commit_rpc() {

		$request = xmlrpc_encode_request($this->method,$this->parms);
		$context = stream_context_create(array('http' => array(
    			'method' => "POST",
    			'header' => "Content-Type: text/xml; charset=utf-8\r\n" .
                	"User-Agent: XMLRPC::Client JorgeRPCclient",
    			'content' => $request
			)));

		$file = file_get_contents("http://$this->rpc_server".":"."$this->rpc_port", false, $context);
		$response = xmlrpc_decode($file,"utf8");
			if (xmlrpc_is_fault($response)) {

				throw new Exception("XML-RPC Call Failed. Unrecoverable condition",0);

			} else {

				return $response;
			}

	}

	protected function is_value($value) {

		if($value === null) {
				return false;
			}
			elseif($value==""){
				return false;
			}
			else{
				return true;
		}


	}

	protected function clean_password($password) {

		if (get_magic_quotes_gpc() === 1) {
			return stripslashes($password);
		}
		
		return $password;
	}

	public function auth() {

		$this->method = "check_password";
		$this->parms = array("user"=>"$this->username","host"=>"$this->vhost","password"=>"$this->password");
		if ($this->commit_rpc() === 0 ) {
			
				return true;

				}
			else{

				return false;

			} 


	}

	public function create_account() {

		if ($this->is_value($this->username) === false OR $this->is_value($this->password) === false) { return false; }
		$this->method = "create_account";
		$this->parms = array("user"=>"$this->username","host"=>"$this->vhost","password"=>"$this->password");
		$call = $this->commit_rpc();
		if ($call === 0) {

					return true;
				}
				elseif($call === 409) {
					return "exist";
				}
				elseif($call === 1) {
					return false;
				}

	}

	public function check_account() {

		if ($this->is_value($this->username) === false) { return false; }
		$this->method = "check_account";
		$this->parms = array("user"=>"$this->username","host"=>"$this->vhost");
		if ($this->commit_rpc() === 1) {
					
					return false;

				}
				else{

					return true;
				
				}	

	}

	public function change_password() {
	
		if ($this->is_value($this->newpass) === false OR $this->is_value($this->username) === false) { return false; }
		$this->method = "change_password";
		$this->parms = array("user"=>"$this->username","host"=>"$this->vhost","newpass"=>"$this->newpass");

		if ($this->commit_rpc() === 0) {
			
					$this->password = $this->newpass;
					return true;

				}
				else{
					return false;

				}

	}

	public function get_roster() {

		$this->method = "get_roster";
		$this->parms = array("user"=>"$this->username","server"=>"$this->vhost");
		return $this->commit_rpc();
	}

	public function delete_account() {
		
		if ($this->is_value($this->password) === false OR $this->is_value($this->username) === false) { return false; }
		$this->method = "delete_account";
		$this->parms = array("user"=>"$this->username","host"=>"$this->vhost","password"=>"$this->password");
		$this->commit_rpc();
		if ($this->check_account() === false) {

						return true;

						}
					else {
						
						return false;
					}
	}

	public function online_users() {
		
		$this->method = "online_users";
		$this->parms = "null";
		return $this->commit_rpc();
	}

	public function test_rpc() {

		$this->method = "echothis";
		$this->parms = "If you can read this then RPC is working...";
		return $this->commit_rpc();

	}


}

?>
