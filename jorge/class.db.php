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

This class provide usefull methods for managing message archives created with mod_logdb.
This is pre-release. Do not use outside project Jorge.

Documentation is available in API.txt.

NOTICE: By default class discards any query with is execuded after query error has occured. This is for debug purposes only.

*/

class db_manager {

	private $db_host;
	private $db_name;
	private $db_user;
	private $db_password;
	private $db_driver;
	private $xmpp_host;
	private $vhost;
	private $messages_table = "logdb_messages_";
	private $is_error = false;
	private $id_query;
	private $query_type;
	private $is_debug = false;
	private $user_id = null;
	private $peer_name_id = null;
	private $peer_server_id = null;
	private $tslice = null;
	private $time_start = null;
	private $time_result = null;
	private $user_query = null;
	private $ignore_id = null;
	private $spec_ignore = false;
	private $ext_idx;
	public $result;

	public function __construct($db_host,$db_name,$db_user,$db_password,$db_driver,$xmpp_host = null) {

		$this->setData($db_host,$db_name,$db_user,$db_password,$db_driver,$xmpp_host);
	
	}


	private function setData($db_host,$db_name,$db_user,$db_password,$db_driver,$xmpp_host) {

		$this->db_host = $db_host;
		$this->db_name = $db_name;
		$this->db_user = $db_user;
		$this->db_password = $db_password;
		$this->db_driver = $db_driver;
		$this->xmpp_host = $this->sql_validate($xmpp_host,"string");
		$this->vhost = str_replace("_",".", $this->sql_validate($xmpp_host,"string"));

		try { 

			$this->db_connect();

			}
		catch(Exception $e) {

        		echo "<br>Exception: ".$e->getMessage();
        		echo ", Code: ".$e->getCode();

		}
		
		if ($this->vhost) {

			$this->set_ignore_id();
		
		}
			
	}

	private function db_mysql() {

		$conn = mysql_connect("$this->db_host", "$this->db_user", "$this->db_password");
		if (!$conn) {

				return false;

			}
		if (mysql_select_db($this->db_name)) {
				
				return true;

				}

			else {

				return false;
			
			}

	}

	private function do_query($query) {

		$this->show_debug_info($query, $time = false);
		if ($this->is_error === false) {

				$this->time_start();
				$result = mysql_query($query);
				$this->time_end();
				$this->show_debug_info($query = null, $time = true);

			}
			elseif($this->is_error === true) {

				if ($this->is_debug === true) {
					if ($this->is_debug === true) {

						throw new Exception("Error before queryID:".$this->id_query,3);
					}
				}
				return false;
		}

		if ($result === false ) {
					
					$this->is_error = true;
					if ($this->is_debug === true) {

						throw new Exception("Query error in QueryID:".$this->id_query,2);
					}
					return false;

				}
			else {

					if ($this->query_type === "select" OR $this->query_type="create_table") {

								return $result;
							}
						elseif($this->query_type === "update" OR $this->query_type === "insert" OR $this->query_type === "replace") {

								return mysql_affected_rows();
						}
						elseif($this->query_type === "delete") {

								return $result;

						}
						elseif($this->query_type === "transaction") {

								return $result;

						}
		}

		return false;

	}

	private function db_query($query) {

		try {

				$result = $this->do_query($query);
			
			}
		catch (Exception $e) {

				echo "<br>Exception: ".$e->getMessage();
				echo ", Code: ".$e->getCode();
				return false;
		}

		return $result;
	}

	private function db_connect() {

		if ($this->db_driver === "mysql") {
			
				if ($this->db_mysql() === true) {

						return true;
					}
				else {
						$this->is_error = true;
						if ($this->is_debug === true) {

							throw new Exception("<br>DB Connection failed!",1);
						}
				}
		}
	
	return false;

	}

	private function select($query,$return_type = null) {

		$this->query_type="select";
		if (strpos(strtolower($query),"select") === 0) {

			try{
				$this->result = $this->db_query($query);
			}
                	catch(Exception $e) {
                        	echo "<br>Exception: ".$e->getMessage();
                        	echo ", Code: ".$e->getCode();
			}

			if($this->is_error===false) {

					if($return_type === null) {

							$this->result = mysql_fetch_object($this->result);

					}
					elseif($return_type === "raw") {

							return true;
				
					}

					return true;	

				}

				else{
					
					return false;
				
				}

		}

		else {

			return false;
		
		}
	}

	private function update($query) {

		$this->query_type = "update";
		if (strpos(strtolower($query),"update") === 0) {

			try{
				$this->result = $this->db_query($query);
			}
                	catch(Exception $e) {
                        	echo "<br>Exception: ".$e->getMessage();
                        	echo ", Code: ".$e->getCode();
			}

			if($this->is_error===false) {
					
					return true;	

				}

				else{
					
					return false;
				
				}

		}

		else {

			return false;
		
		}
	}

	private function insert($query) {

		$this->query_type = "insert";
		if (strpos(strtolower($query),"insert") === 0) {

			try{
				$this->result = $this->db_query($query);
			}
                	catch(Exception $e) {
                        	echo "<br>Exception: ".$e->getMessage();
                        	echo ", Code: ".$e->getCode();
			}

			if($this->is_error===false) {
					
					return true;	

				}

				else{
					
					return false;
				
				}

		}

		else {

			return false;
		
		}
	}

	private function delete($query) {

		$this->query_type = "delete";
		if (strpos(strtolower($query),"delete") === 0) {

			try{
				$this->result = $this->db_query($query);
			}
			catch(Exception $e) {
				echo "<br>Exception: ".$e->getMessage();
				echo ", Code: ".$e->getCode();
			}

			if ($this->is_error===false) {

					return true;

				}
				else {

					return false;

				}

		}
		else {

			return false;

		}
	}

	private function replace_q($query) {

		$this->query_type = "replace";
		if (strpos(strtolower($query),"replace") === 0) {

			try{
				$this->result = $this->db_query($query);
			}
			catch(Exception $e) {
				echo "<br>Exception: ".$e->getMessage();
				echo ", Code: ".$e->getCode();
			}

			if ($this->is_error === false) {

					return true;

				}
				else {

					return false;

				}

		}
		else {

			return false;

		}
	}

	public function begin() {

		$this->id_query = "Q001";
		$this->query_type = "transaction";
		return $this->db_query("begin");

	}
	
	public function commit() {
		
		$this->id_query = "Q002";
		$this->query_type = "transaction";
		return $this->db_query("commit");
	
	}

	public function rollback() {

		$this->id_query = "Q003";
		$this->query_type = "transaction";
		$this->is_error = false;
		return $this->db_query("rollback");

	}

	public function get_mylinks_count() {

		$this->id_query = "Q004";
		$this->vital_check();
		$query="SELECT 
				count(id_link) as cnt
			FROM 
				jorge_mylinks 
			WHERE 
				owner_id='".$this->user_id."'
			AND
				vhost='".$this->vhost."'
			AND 
				ext is NULL
		
		";
		
		return $this->select($query);
	}

	public function get_trash_count() {
	
		$this->id_query = "Q005";
		$this->vital_check();
		$query="SELECT 
				count(*) as cnt
			FROM 
				pending_del 
			WHERE 
				owner_id='".$this->user_id."'
			AND
				vhost='".$this->vhost."'
		";

		return $this->select($query);
		
	}

	private function row_count($query) {

		$this->id_query = "Q006";
		$result = mysql_num_rows($this->db_query($query));
		if ($result === false) {

				return false;
			
			}
			else{

				$this->result = $result;
				return true;
		
			}
		
	}

	public function get_user_id($user) {
		
		$this->id_query = "Q007";	
		$user = $this->sql_validate($user,"string");
		$query="SELECT
				user_id 
			FROM 
				`logdb_users_".$this->xmpp_host."`
			WHERE 
				username = '$user'
				
			";
		
		return $this->select($query);

	}

	public function get_user_name($user_id,$vhost = null) {

		$this->id_query = "Q008";
		$user_id = $this->sql_validate($user_id,"integer");
		if ($vhost !== null) {

				$vh = $this->vh($vhost,true);

			}
			else{

				$vh = $this->xmpp_host;

		
		}
		$query="SELECT
				username
			FROM 
				`logdb_users_".$vh."`
			WHERE 
				user_id = '$user_id'
				
			";
		
		return $this->select($query);

	}

	public function get_server_id($server) {

		$this->id_query = "Q009";
		$server = $this->sql_validate($server,"string");
		$query="SELECT
				server_id 
			FROM 
				`logdb_servers_".$this->xmpp_host."`
			WHERE 
				server = '$server'
				
			";
		
		return $this->select($query);

	}

	public function get_server_name($server_id,$vhost = null){

		$this->id_query = "Q010";
		$server_id = $this->sql_validate($server_id,"integer");
		if ($vhost !== null) {

				$vh = $this->vh($vhost,true);

			}
			else{

				$vh = $this->xmpp_host;

		
		}
		$query="SELECT
				server as server_name
			FROM 
				`logdb_servers_".$vh."` 
			WHERE 
				server_id = '$server_id'
				
			";
		
		return $this->select($query);

	}

	public function get_resource_name($resource_id) {

		$this->id_query = "Q012";
		$resource_id = $this->sql_validate($resource_id,"integer");
		$query="SELECT
				resource as resource_name
			FROM 
				`logdb_resources_".$this->xmpp_host."` 
			WHERE 
				resource_id = '$resource_id'
				
			";
		
		return $this->select($query);
	}

	public function get_resource_id($resource) {
	
		$this->id_query = "Q013";
		$resource = $this->sql_validate($resource,"string");
		$query="SELECT
				resource_id
			FROM 
				`logdb_resources_".$this->xmpp_host."`
			WHERE 
				resource = '$resource'
				
			";
		
		return $this->select($query);

	}

	public function get_user_talker_stats($peer_name_id,$peer_server_id){

		$this->id_query = "Q014";
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		$query="SELECT
				at 
			FROM 
				`logdb_stats_".$this->xmpp_host."`
			WHERE 
				owner_id='".$this->user_id."' 
			AND 
				peer_name_id='".$this->peer_name_id."' 
			AND 
				peer_server_id='".$this->peer_server_id."' 
			ORDER BY
				str_to_date(at,'%Y-%m-%d') 
			ASC
			
			";
		
		$this->select($query,"raw");
		return $this->commit_select(array("at"));
	}

	public function get_num_lines($tslice,$peer_name_id,$peer_server_id) {
		
		$this->id_query = "Q015";
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		$table = $this->construct_table($this->tslice);
		$query="SELECT 
				count(timestamp) as cnt
			FROM 
				`$table` 
			WHERE 
				owner_id = '".$this->user_id."' 
			AND 
				peer_name_id='".$this->peer_name_id."' 
			AND 
				peer_server_id='".$this->peer_server_id."'
				
		";
		
		return $this->select($query);

	}

	public function is_log_enabled() {

		$this->id_query = "Q016";

		// Return false on non-digit characters, workaround for user not being in sql dictionary.
		if (!ctype_digit($this->user_id)) {

			return false;

		}

		$query="SELECT 
				dolog_default as is_enabled
			FROM 
				`logdb_settings_".$this->xmpp_host."` 
			WHERE 
				owner_id='".$this->user_id."'
		";
		
		if ($this->select($query) === true) {

				if ($this->result->is_enabled === "0") {

						$this->result->is_enabled = false;
						return true;
					
					}
					elseif($this->result->is_enabled === "1") {

						$this->result->is_enabled = true;
						return true;
			
					}
					else{

						 $this->result->is_enabled = null;
						 return true;

				}



		}

		return false;
	
	}

	public function total_messages($vhost = null) {
	
		$this->id_query = "Q017";
		if ($vhost === null) {

				$vh = $this->xmpp_host;

			}
			else{
				
				$vh = $this->vh($vhost,true);

		}
		$query="SELECT 
				sum(count) as total_messages,
				count(owner_id) as total_chats
			FROM 
				`logdb_stats_".$vh."`
		";
		
		$this->select($query,"raw");
		return $this->commit_select(array("total_messages","total_chats"));

	}

	public function total_chats() {

		$this->id_query = "Q018";
		$query="SELECT 
				count(owner_id) as total_chats
			FROM 
				`logdb_stats_".$this->xmpp_host."`
		";
		return $this->select($query);

	}

	public function get_log_list() {

		$this->id_query = "Q019";
		$this->vital_check();
		$query="SELECT 
				donotlog_list as donotlog
			FROM 
				`logdb_settings_".$this->xmpp_host."` 
			WHERE 
				owner_id = '".$this->user_id."'
		";

		$this->select($query);
		$split = explode("\n",$this->result->donotlog);
		$this->result = $split;
		return true;

	}

	public function set_log($bool) {

		$this->id_query = "Q020";
		$this->vital_check();
		if ($bool === true) {

				$val = 1;
			
			}
			elseif($bool === false) {

				$val = 0;
			
			}
			else{

				return false;
		}
		
		$query="UPDATE 
				`logdb_settings_".$this->xmpp_host."`
			SET 	
				dolog_default = '$val' 
			WHERE 
				owner_id = '".$this->user_id."'
		";

		return $this->update($query);

	}

	public function set_logger($event_id,$event_level,$extra = null) {
		
		$this->id_query = "Q021";
		$this->vital_check();
		$id_log_detail = $this->sql_validate($event_id,"integer");
		$id_log_level = $this->sql_validate($event_level,"integer");
		$extra = $this->sql_validate($extra,"string");
		$query="INSERT INTO 
				jorge_logger (id_user,id_log_detail,id_log_level,log_time,extra,vhost) 
			VALUES 
				('".$this->user_id."','$id_log_detail','$id_log_level',NOW(),'$extra','".$this->vhost."')
				
		";

		return $this->insert($query);
	}

	public function get_user_stats_drop_down() {

		$this->id_query = "Q022";
		$this->vital_check();
		if ($this->spec_ignore === true) {

			$sql = "AND peer_name_id != '".$this->ignore_id."'";

		}
		$query="SELECT 
				substring(at,1,7) as at_send, 
				at 
			FROM 
				`logdb_stats_".$this->xmpp_host."` 
			WHERE 
				owner_id = '".$this->user_id."' 
				$sql
			GROUP BY 
				substring(at,1,7) 
			ORDER BY 
				str_to_date(at,'%Y-%m-%d') 
			DESC
		
		";

		$this->select($query,"raw");
		return $this->commit_select(array("at_send","at"));
	}

	public function get_user_stats_calendar($mo) {

		$this->id_query = "Q023";
		$this->vital_check();
		$mo = $this->sql_validate($mo,"string");
		if ($this->spec_ignore === true) {

			$sql = "AND peer_name_id != '".$this->ignore_id."'";

		}
		$query="SELECT 
				distinct(substring(at,8,9)) as days 
			FROM 
				`logdb_stats_".$this->xmpp_host."` 
			WHERE 
				owner_id = '".$this->user_id."' 
			AND
				at like '$mo-%' 
				$sql
			ORDER BY 
				str_to_date(at,'%Y-%m-%d') 
			DESC
			
		";

		$this->select($query,"raw");
		return $this->commit_select(array("days"));
	}

	public function get_user_chats($tslice) {
		
		$this->id_query = "Q024";
		$this->vital_check();
		$xmpp_host = $this->xmpp_host;
		$tslice_table = $this->sql_validate($tslice,"string");
		$query="SELECT 
				a.username, 
				b.server as server_name, 
				c.peer_name_id as todaytalk, 
				c.peer_server_id as server, 
				c.count as lcount 
			FROM 
				`logdb_users_$xmpp_host` a, 
				`logdb_servers_$xmpp_host` b, 
				`logdb_stats_$xmpp_host` c 
			WHERE 
				c.owner_id = '".$this->user_id."' 
			AND 
				a.user_id=c.peer_name_id 
			AND 
				b.server_id=c.peer_server_id 
			AND 
				c.at = '$tslice' 
			ORDER BY 
				lower(username)
				
		";

		$this->select($query,"raw");
		return $this->commit_select(array("username","server_name","todaytalk","server","lcount"));

	}

	public function get_user_chat($tslice,$peer_name_id,$peer_server_id,$resource_id = null,$start = null,$num_lines = null) {
	
		$this->id_query = "Q025";
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		if ($resource_id !== null) { 
		
				$resource_id = $this->sql_validate($resource_id,"integer");
				$sql = "AND (peer_resource_id='$resource_id' OR peer_resource_id='1')";

			}
			else{

				settype($sql,"null");
			}

		$offset_start = $start;
		if ($offset_start === null) {

				$offset_start = "0";

			}

		$offset_end = $start + $num_lines;
		$offset_start = $this->sql_validate($offset_start,"integer");
		$offset_end = $this->sql_validate($offset_end,"integer");
		$tslice_table = $this->construct_table($this->tslice);
		$query="SELECT 
				from_unixtime(timestamp+0) as ts,
				direction,
				type,
				subject,
				peer_name_id, 
				peer_server_id, 
				peer_resource_id, 
				body 
			FROM 
				`$tslice_table` 
			FORCE INDEX
				(search_i)
			WHERE 
				owner_id = '".$this->user_id."' 
			AND 
				peer_name_id='".$this->peer_name_id."' 
			AND 
				peer_server_id='".$this->peer_server_id."' 
				$sql 
			AND 
				ext is NULL 
			ORDER BY 
				ts 
			LIMIT 
				$offset_start,$offset_end
		";

		$this->select($query,"raw");
		return $this->commit_select(array("ts","direction","type","subject","peer_name_id","peer_server_id","peer_resource_id","body"));

	}

	public function get_uniq_chat_dates($limit_start = null, $limit_end = null, $limited = false, $start = null, $peer_name_id = null, $peer_server_id = null) {

		$this->id_query = "Q026";
		$this->vital_check();
		$user_id = $this->user_id;
		$xmpp_host = $this->xmpp_host;
		if ($limit_start !== null AND $limit_end !== null) {
	
				$limit_start = $this->sql_validate($limit_start,"date");
				$limit_end = $this->sql_validate($limit_end,"date");
				$sql=" AND str_to_date(at,'%Y-%m-%d') >= str_to_date('$limit_start','%Y-%m-%d') AND str_to_date(at,'%Y-%m-%d') <= str_to_date('$limit_end','%Y-%m-%d')";
				
			}
			else{

				settype($sql,"null");

		}

		if ($limited === true) {

				if ($start == "" OR $start === null) { 
				
					$start = "0"; 
					
				}

				$start = $this->sql_validate($start,"integer");
				$sql2=" limit $start,10000";

			}
			else{

				settype($sql2,"null");
		
		}

		if ($peer_name_id !== null AND $peer_server_id !== null) {

				$peer_name_id = $this->sql_validate($peer_name_id,"integer");
				$peer_server_id = $this->sql_validate($peer_server_id,"integer");
				$sql3 = "AND peer_name_id = '$peer_name_id' AND peer_server_id = '$peer_server_id'";

			}
			else{

				settype($sql3,"null");

		}
		
		$query="SELECT 
				distinct(at) 
			FROM 
				`logdb_stats_$xmpp_host` 
			WHERE 
				owner_id='$user_id' $sql3 $sql
			ORDER BY 
				str_to_date(at,'%Y-%m-%d') 
			ASC
				$sql2
		";

		$this->select($query,"raw");
		return $this->commit_select(array("at"));

	}

	public function check_thread($tslice,$peer_name_id,$peer_server_id,$begin_hour,$end_hour) {

		$this->id_query = "Q027";
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		$xmpp_host = $this->xmpp_host;
		$tslice_table = $this->construct_table($this->tslice);
        	$query="SELECT 
				1 
                	FROM 
                        	`$tslice_table`
                	WHERE 
                        	owner_id='".$this->user_id."' 
                	AND 
                        	peer_name_id='".$this->peer_name_id."' 
                	AND 
                        	peer_server_id='".$this->peer_server_id."' 
                	AND 
                        	from_unixtime(timestamp) >= str_to_date('".$this->tslice." $begin_hour','%Y-%m-%d %H:%i:%s') 
                	AND 
                        	from_unixtime(timestamp) <= str_to_date('".$this->tslice." $end_hour','%Y-%m-%d %H:%i:%s')
                	ORDER BY 
                        	from_unixtime(timestamp)

		";
		
		return $this->row_count($query);

	}

	public function get_chat_map($peer_name_id,$peer_server_id) {

		$this->id_query = "Q028";
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		$query="SELECT 
				substring(at,1,7) as at 
			FROM 
				`logdb_stats_".$this->xmpp_host."` 
			WHERE 
				owner_id='".$this->user_id."' 
			AND 
				peer_name_id='".$this->peer_name_id."' 
			AND 
				peer_server_id='".$this->peer_server_id."' 
			GROUP BY 
				substring(at,1,7) 
			ORDER BY 
				str_to_date(at,'%Y-%m-%d') 
			ASC
			
		";

		$this->select($query,"raw");
		return $this->commit_select(array("at"));

	}

	public function get_chat_map_specyfic($peer_name_id,$peer_server_id,$month) {

		$this->id_query = "Q029";
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		$mo = $this->sql_validate($month,"string");
		$query="SELECT 
				at 
			FROM 
				`logdb_stats_".$this->xmpp_host."` 
			WHERE 
				owner_id='".$this->user_id."' 
			AND 
				peer_name_id='".$this->peer_name_id."' 
			AND 
				peer_server_id='".$this->peer_server_id."' 
			AND 
				at like '$mo-%'
				
		";

		$this->select($query,"raw");
		return $this->commit_select(array("at"));

	}

	public function add_mylink($peer_name_id,$peer_server_id,$link_date,$link,$desc) {

		$this->id_query = "Q030";
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		$datat = $this->sql_validate($link_date,"string");
		$lnk = $this->sql_validate($link,"string");
		$desc = $this->sql_validate($desc,"string");
		$query="INSERT INTO
				jorge_mylinks (owner_id,peer_name_id,peer_server_id,datat,link,description,vhost) 
			VALUES (
					'".$this->user_id."',
					'".$this->peer_name_id."',
					'".$this->peer_server_id."',
					'$datat',
					'$lnk',
					'$desc',
					'".$this->vhost."'
				)
				
		";

		return $this->insert($query);

	}

	public function del_mylink($link_id) {

		$this->id_query = "Q031";
		$this->vital_check();
		$link_id = $this->sql_validate($link_id,"integer");
		$query="DELETE FROM 
				jorge_mylinks 
			WHERE 
				owner_id='".$this->user_id."' 
			AND
				vhost='".$this->vhost."'
			AND 
				id_link='$link_id'
				
		";

		return $this->delete($query);

	}

	public function get_mylink() {

		$this->id_query = "Q032";
		$this->vital_check();
		$query="SELECT
				id_link,
				peer_name_id,
				peer_server_id,
				datat,
				link,
				description,
				ext
			FROM 
				jorge_mylinks 
			WHERE 
				owner_id='".$this->user_id."' 
			AND
				vhost='".$this->vhost."'
			AND 
				ext is NULL 
			ORDER BY 
				str_to_date(datat,'%Y-%m-%d') 
			DESC
			
		";

		$this->select($query,"raw");
		return $this->commit_select(array("id_link","peer_name_id","peer_server_id","datat","link","description","ext"));


	}

	public function update_log_list($log_list) {

		$this->id_query = "Q033";
		$this->vital_check();
		$log_list = $this->sql_validate($log_list,"string");
		$query="UPDATE 
				`logdb_settings_".$this->xmpp_host."` 
			SET 
				donotlog_list='$log_list' 
			WHERE 
				owner_id='".$this->user_id."'
		";
		return $this->update($query);

	}

	public function logger_get_events($event_id = null,$level_id = null, $offset = null,$lang = null) {

		$this->id_query = "Q034";
		$this->vital_check();
		$offset = $this->sql_validate($offset,"integer");
		if ($event_id !== null) {
				
				$event_id = $this->sql_validate($event_id,"integer");
				$sql_1 = "and id_log_detail='$event_id'";

		}
		if ($level_id !== null) {

				$level_id = $this->sql_validate($level_id,"integer");
				$sql_2 = "and id_log_level='$level_id'";
		
		}
		$query="SELECT 
				b.id_event, 
				b.event AS event,
				c.level AS level, 
				c.id_level, 
				a.log_time,
				a.extra
			FROM 
				jorge_logger a,
				jorge_logger_dict b,
				jorge_logger_level_dict c
			WHERE 
				a.id_log_detail=b.id_event 
			AND 
				c.id_level=a.id_log_level 
			AND 
				id_user='".$this->user_id."' 
			AND
				a.vhost='".$this->vhost."'
			AND
				b.lang = '$lang'
			AND
				c.lang = '$lang'

			$sql_1 
			$sql_2

			ORDER BY 
				log_time 
			DESC LIMIT 
				$offset,300
		";
		
		$this->select($query,"raw");
		return $this->commit_select(array("id_event","event","level","id_level","log_time","extra"));

	}

	public function get_num_events($event_id = null,$level_id = null) {

		$this->id_query = "Q035";
		$this->vital_check();
		if ($event_id !== null) {
				
				$event_id = $this->sql_validate($event_id,"integer");
				$sql_1 = "AND id_log_detail='$event_id'";
		}
		if ($level_id !== null) {

				$level_id = $this->sql_validate($level_id,"integer");
				$sql_2 = "AND id_log_level='$level_id'";
		}
		$query="SELECT 
				count(id_user) AS cnt
			FROM 
				jorge_logger 
			WHERE 
				id_user='".$this->user_id."' 
			AND
				vhost='".$this->vhost."'
			
			$sql_1 
			$sql_2
		";

		return $this->select($query);

	}

	public function get_trashed_items() {

		$this->id_query = "Q036";
		$this->vital_check();
		$query="SELECT 
				peer_name_id,
				peer_server_id,
				date,
				timeframe,
				type,
				idx
			FROM 
				pending_del 
			WHERE 
				owner_id = '".$this->user_id."' 
			AND
				vhost='".$this->vhost."'
			ORDER BY 
				str_to_date(date,'%Y-%m-%d') 
			DESC
		";

		$this->select($query,"raw");
		return $this->commit_select(array("peer_name_id","peer_server_id","date","timeframe","type","idx"));

	}

	public function move_chat_to_trash($peer_name_id,$peer_server_id,$tslice,$link) {

		$this->id_query = "Q037";
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		$xmpp_host = $this->xmpp_host;
		$table = $this->construct_table($this->tslice);

		if ($this->get_ext_index($table) !== true) {

				return false;

			}
			else{

				if (!$this->result->idx) {

						$this->ext_idx = "1";

					}
					else{

						$this->ext_idx = $this->result->idx;

				}

		}

		$this->begin();
		if ($this->set_undo_table($this->peer_name_id,$this->peer_server_id,$this->tslice,"chat") === false) {

				$this->rollback();
				return false;

		}

		if ($this->remove_user_stats($this->peer_name_id,$this->peer_server_id,$this->tslice) === false) {

				$this->rollback();
				return false;

		}

		if ($this->move_mylink_to_trash($peer_name_id,$link) === false) {

				$this->rollback();
				return false;

		}

		if ($this->move_fav_to_trash($peer_name_id,$peer_server_id,$tslice) === false) {

				$this->rollback();
				return false;

		}

		$this->id_query = "Q037a";
		$query="UPDATE 
				`$table` 
			SET 
				ext = '".$this->ext_idx."' 
			WHERE 
				owner_id='".$this->user_id."' 
			AND 
				peer_name_id='".$this->peer_name_id."' 
			AND 
				peer_server_id='".$this->peer_server_id."'
			AND
				ext is NULL
				
		";
		
		if ($this->update($query) === false) {
				
				$this->rollback();
				return false;

			}
			else{

				$this->commit();
				$this->set_logger("4","1");
				return true;
		}
	}

	private function remove_user_stats($peer_name_id,$peer_server_id,$tslice) {

		$this->id_query = "Q038";
		$query="DELETE FROM 
				`logdb_stats_".$this->xmpp_host."` 
			WHERE 
				owner_id='".$this->user_id."' 
			AND 
				peer_name_id='$peer_name_id' 
			AND 
				peer_server_id='$peer_server_id' 
			AND 
				at='$tslice'
		";

		return $this->delete($query);
	
	}

	public function move_mylink_to_trash($peer_name_id,$link) {

		$this->id_query = "Q039";
		$this->vital_check();
		$peer_name_id = $this->sql_validate($peer_name_id,"integer");
		$lnk = $this->sql_validate($link,"string");
		$query="UPDATE 
				jorge_mylinks 
			SET 
				ext='".$this->ext_idx."' 
			WHERE 
				owner_id ='".$this->user_id."' 
			AND
				vhost='".$this->vhost."'
			AND 
				peer_name_id='$peer_name_id' 
			AND 
				link like '$lnk%'
			AND
				ext is NULL
		";

		return $this->update($query);

	}

	public function move_fav_to_trash($peer_name_id,$peer_server_id,$tslice) {

		$this->id_query = "Q040";
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		$query="UPDATE 
				jorge_favorites 
			SET 
				ext='".$this->ext_idx."' 
			WHERE 
				owner_id='".$this->user_id."' 
			AND 
				peer_name_id='".$this->peer_name_id."' 
			AND 
				peer_server_id='".$this->peer_server_id."' 
			AND 
				tslice='".$this->tslice."'
			AND
				vhost='".$this->vhost."'
			AND
				ext is NULL
		";
	
		return $this->update($query);

	}

	private function set_undo_table($peer_name_id,$peer_server_id,$tslice,$type = null) {

		$this->id_query = "Q041";
		$query="INSERT INTO 
				pending_del(owner_id,peer_name_id,date,peer_server_id,type,idx,vhost) 
			VALUES (
				'".$this->user_id."', 
				'$peer_name_id',
				'$tslice',
				'$peer_server_id',
				'$type',
				'".$this->ext_idx."',
				'".$this->vhost."'
				)
				
		";
		
		return $this->insert($query);

	}

	private function unset_undo_table($peer_name_id,$peer_server_id,$tslice,$type = null) {

		$this->id_query = "Q042";
		$query="DELETE FROM 
				pending_del 
			WHERE 
				owner_id='".$this->user_id."' 
			AND 
				peer_name_id='$peer_name_id' 
			AND 
				date='$tslice' 
			AND 
				peer_server_id='$peer_server_id'
			AND
				idx = '".$this->ext_idx."'
			AND
				vhost='".$this->vhost."'
		";

		return $this->delete($query);
	}

	public function move_chat_from_trash($peer_name_id,$peer_server_id,$tslice,$link,$idx = null) {

		$this->id_query = "Q043";
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		$xmpp_host = $this->xmpp_host;
		$table = $this->construct_table($this->tslice);
		if (!$idx) {

				$this->ext_idx = "1";
			}
			else{

				if (ctype_digit($idx)) {

						$this->ext_idx = $idx;

					}
					else{

						return false;

				}

		}

		// Message tables are not transactional, so this make some trouble for us to control all error conditions :/
		$query="UPDATE 
				`$table` 
			SET 
				ext = NULL 
			WHERE 
				owner_id='".$this->user_id."' 
			AND 
				peer_name_id='".$this->peer_name_id."' 
			AND 
				peer_server_id='".$this->peer_server_id."'
			AND
				ext = '".$this->ext_idx."'
		";

		if ($this->update($query) === false) {

				return false; 

		}

		$this->begin();
		if ($this->unset_undo_table($this->peer_name_id,$this->peer_server_id,$this->tslice) === false) {

				$this->rollback();
				return false;

		}

		if ($this->recount_messages($this->peer_name_id,$this->peer_server_id,$this->tslice) === true) {

				$stats = $this->result->cnt;

			}
			else {

				$this->rollback();
				return false;
		}

		if ($this->if_chat_exist($this->peer_name_id,$this->peer_server_id,$this->tslice) === true) {


					if ($this->result->cnt == 1) {

							if ($this->update_stats($this->peer_name_id,$this->peer_server_id,$this->tslice,$stats) === false) {

									$this->rollback();
									return false;
							}

						}
						else {

							if ($this->insert_stats($this->peer_name_id,$this->peer_server_id,$this->tslice,$stats) === false) {

									$this->rollback();
									return false;

							}
						}

			}
			else{

					$this->rollback();
					return false;
		}

		if ($this->move_mylink_from_trash($peer_name_id,$link) === false) {

				$this->rollback();
				return false;

		}

		if ($this->move_fav_from_trash($peer_name_id,$peer_server_id,$tslice) === false) {

				$this->rollback();
				return false;
		}

		$this->commit();
		return true;
	
	
	}

	private function if_chat_exist($peer_name_id,$peer_server_id,$tslice) {

		$this->id_query = "Q044";
		$query="SELECT 
				1 as cnt
			FROM 
				`logdb_stats_".$this->xmpp_host."` 
			WHERE 
				owner_id = '".$this->user_id."' 
			AND 
				peer_name_id='$peer_name_id' 
			AND 
				peer_server_id='$peer_server_id' 
			AND 
				at = '$tslice'
				
		";

		return $this->select($query);

	}

	private function insert_stats($peer_name_id,$peer_server_id,$tslice,$stats) {
	
		$this->id_query = "Q045";
		$query="INSERT INTO
				`logdb_stats_".$this->xmpp_host."` (owner_id,peer_name_id,peer_server_id,at,count) 
			VALUES 
				(
				'".$this->user_id."',
				'$peer_name_id',
				'$peer_server_id',
				'$tslice',
				'$stats
				')
				
		"; 
		
		return $this->insert($query);
	}

	private function update_stats($peer_name_id,$peer_server_id,$tslice,$stats) {

		$this->id_query = "Q046";
		$query="UPDATE 
				`logdb_stats_".$this->xmpp_host."` 
			SET 
				count='$stats' 
			WHERE 
				owner_id='".$this->user_id."' 
			AND 
				peer_name_id='$peer_name_id' 
			AND 
				peer_server_id='$peer_server_id' 
			AND 
				at='$tslice'
				
		";

		return $this->update($query);
	}

	private function recount_messages($peer_name_id,$peer_server_id,$tslice) {
	
		$this->id_query = "Q047";
		$table = $this->construct_table($tslice);
		$query="SELECT
				count(timestamp) as cnt 
			FROM 
				`$table`
			WHERE 
				owner_id='".$this->user_id."' 
			AND 
				peer_name_id='$peer_name_id' 
			AND 
				peer_server_id='$peer_server_id' 
			AND 
				ext is NULL
		";
		
		return $this->select($query);

	}

	private function move_mylink_from_trash($peer_name_id,$link) {

		$this->id_query = "Q048";
		$lnk = $this->sql_validate($link,"string");
		$query="UPDATE 
				jorge_mylinks 
			SET 
				ext = NULL 
			WHERE 
				owner_id ='".$this->user_id."' 
			AND
				vhost='".$this->vhost."'
			AND 
				peer_name_id='$peer_name_id' 
			AND
				ext = '".$this->ext_idx."'
			AND 
				link like '$link%'
		";

		return $this->update($query);

	}

	private function move_fav_from_trash($peer_name_id,$peer_server_id,$tslice) {

		$this->id_query = "Q049";
		$query="UPDATE 
				jorge_favorites 
			SET 
				ext = NULL
			WHERE 
				owner_id='".$this->user_id."' 
			AND 
				peer_name_id='$peer_name_id' 
			AND 
				peer_server_id='$peer_server_id' 
			AND 
				tslice='$tslice'
			AND
				ext = '".$this->ext_idx."'
			AND
				vhost='".$this->vhost."'
		";
	
		return $this->update($query);

	}

	public function delete_messages($peer_name_id,$peer_server_id,$tslice) {

		$this->id_query = "Q050";
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		$table = $this->construct_table($this->tslice,"date");
		$query="DELETE FROM 
				`$table`
			WHERE 
				owner_id='".$this->user_id."' 
			AND 
				peer_name_id='".$this->peer_name_id."' 
			AND 
				peer_server_id='".$this->peer_server_id."' 
			AND 
				ext = '".$this->ext_idx."'
				
		";

		return $this->delete($query);

	}

	public function delete_mylinks($peer_name_id,$peer_server_id,$tslice) {
	
		$this->id_query = "Q051";
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		$query="DELETE FROM
				jorge_mylinks 
			WHERE 
				owner_id='".$this->user_id."' 
			AND
				vhost='".$this->vhost."'
			AND 
				ext='".$this->ext_idx."' 
			AND 
				peer_name_id = '".$this->peer_name_id."' 
			AND 
				peer_server_id='".$this->peer_server_id."' 
			AND 
				datat = '".$this->tslice."'
				
		";

		return $this->delete($query);

	}

	public function delete_favorites($peer_name_id,$peer_server_id,$tslice) {

		$this->id_query = "Q052";
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		$query="DELETE FROM 
				jorge_favorites 
			WHERE 
				owner_id='".$this->user_id."' 
			AND 
				peer_name_id='".$this->peer_name_id."' 
			AND 
				peer_server_id='".$this->peer_server_id."' 
			AND 
				tslice='".$this->tslice."'
			AND
				ext = '".$this->ext_idx."'
			AND
				vhost='".$this->vhost."'
		";

		return $this->delete($query);

	}

	public function search_query($tslice) {

		$this->id_query = "Q053";
		$this->vital_check();
		if ($this->user_query === null) {

				return false;

			}
		$table = $this->construct_table($this->sql_validate($tslice,"date"));
		$query="SELECT
				timestamp AS ts, 
				peer_name_id, 
				peer_server_id, 
				direction, 
				ext, 
				body, 
				MATCH(body) AGAINST('".$this->user_query."' IN BOOLEAN MODE) AS score
			FROM 
				`$table`
			FORCE INDEX
				(search_i)
			WHERE 
				MATCH(body) AGAINST('".$this->user_query."' IN BOOLEAN MODE) 
			AND 
				owner_id='".$this->user_id."' 
			LIMIT 
				0,10000
		";

		$this->select($query,"raw");
		return $this->commit_select(array("ts","peer_name_id","peer_server_id","direction","ext","body","score"));

	}

	public function search_query_chat_stream($peer_name_id,$peer_server_id,$tslice,$start_tag = null) {
		
		$this->id_query = "Q054";
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		$table = $this->construct_table($this->sql_validate($this->tslice,"date"));
		if ($start_tag === null) {

				$start_tag="0";
		}
		$start_tag = $this->sql_validate($start_tag,"integer");

		$query="SELECT 
				from_unixtime(timestamp+0) AS ts, 
				peer_name_id, 
				peer_server_id, 
				direction, 
				ext, 
				body 
			FROM 
				`$table` 
			FORCE INDEX
				(search_i)
			WHERE 
				owner_id='".$this->user_id."' 
			AND 
				peer_name_id='".$this->peer_name_id."' 
			AND 
				peer_server_id='".$this->peer_server_id."' 
			LIMIT 
				$start_tag,10000
		";

		$this->select($query,"raw");
		return $this->commit_select(array("ts","peer_name_id","peer_server_id","direction","ext","body"));
	
	}

	public function search_query_in_user_chat($peer_name_id,$peer_server_id,$tslice,$start_tag) {
		
		$this->id_query = "Q055";
		if ($this->user_query === null) {

				return false;

			}
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		$table = $this->construct_table($this->sql_validate($this->tslice,"date"));
		if ($start_tag === null) {

				$start_tag="0";
		}
		$start_tag = $this->sql_validate($start_tag,"integer");
		$query="SELECT
				timestamp AS ts, 
				peer_name_id, 
				peer_server_id, 
				direction, 
				ext, 
				body ,
				MATCH(body) AGAINST('".$this->user_query."' IN BOOLEAN MODE) AS score 
			FROM
				`$table` 
			FORCE INDEX
				(search_i)
			WHERE 
				match(body) against('".$this->user_query."' IN BOOLEAN MODE) 
			AND 
				owner_id='".$this->user_id."' 
			AND 
				peer_name_id='".$this->peer_name_id."' 
			AND
				peer_server_id='".$this->peer_server_id."' 
			LIMIT 
				$start_tag,10000
		";

		$this->select($query,"raw");
		return $this->commit_select(array("ts","peer_name_id","peer_server_id","direction","ext","body","score"));

	}

	public function create_search_results_table() {

		$this->id_query = "Q055";
		$this->query_type = "create_table";
		$query="CREATE TEMPORARY TABLE 
				jorge_results_table 
				(
					ts VARCHAR(30),
					time_slice VARCHAR(10),
					peer_name_id MEDIUMINT,
					peer_server_id SMALLINT,
					direction ENUM('to','from'),
					body TEXT,
					score FLOAT,
					ext TINYINT
				)
		";
		
		return $this->db_query($query);
	}

	public function insert_data_to_result_table($ts,$time_slice,$peer_name_id,$peer_server_id,$direction,$body,$score,$ext){

		$this->id_query = "Q056";
		$query="INSERT INTO jorge_results_table 
				(ts,time_slice,peer_name_id,peer_server_id,direction,body,score,ext) 
			VALUES (
                                        '$ts',
                                        '$time_slice',
                                        '$peer_name_id',
                                        '$peer_server_id',
                                        '$direction',
                                        '$body',
                                        '$score',
                                        '$ext'
				)
		";

		return $this->insert($query);

	}

	public function get_search_results() {

		$this->id_query = "Q057";
		$query="SELECT 
				FROM_UNIXTIME(ts+0) AS ts, 
				time_slice, 
				peer_name_id, 
				peer_server_id, 
				direction, 
				body, 
				score, 
				ext 
			FROM
				jorge_results_table 
			ORDER BY 
				score 
			DESC LIMIT 100
			
		";

		$this->select($query,"raw");
		return $this->commit_select(array("ts","time_slice","peer_name_id","peer_server_id","direction","body","score","ext"));

	}

	public function get_folder_content($at) {

		$this->id_query = "Q058";
		$this->vital_check();
		$at = $this->sql_validate($at,"string");
                if ($this->spec_ignore === true) {

                        $sql = "AND peer_name_id != '".$this->ignore_id."'";

                }
		$query="SELECT 
				distinct(at) AS at 
			FROM 
				`logdb_stats_".$this->xmpp_host."` 
			WHERE 
				owner_id = '".$this->user_id."' 
			AND 
				substring(at,1,7) = '$at' 
				$sql
			ORDER BY 
				str_to_date(at,'%Y-%m-%d') 
			DESC
		";

		$this->select($query,"raw");
		return $this->commit_select(array("at"));

	}

	public function insert_user_id($user_name) {

		$this->id_query = "Q059";
		$user_name = $this->sql_validate($user_name,"string");
		$query="INSERT INTO 
				`logdb_users_".$this->xmpp_host."` 
			SET 
				username='$user_name'
		";
		return $this->insert($query);

	}

	public function insert_new_settings($user_name) {

		$this->id_query = "Q060";
		$user_name = $this->sql_validate($user_name,"string");
		$query="INSERT INTO 
				`logdb_settings_".$this->xmpp_host."` (owner_id,dolog_default) 
			VALUES 
				((SELECT user_id FROM `logdb_users_".$this->xmpp_host."` WHERE username='$user_name'), '1')
				
		";
		return $this->insert($query);

	}

	public function get_jorge_pref($pref_id = null) {

		$this->id_query = "Q061";
		if ($pref_id !== null) {

			$pref_id = $this->sql_validate($pref_id,"integer");
			$sql = "AND pref_id = '$pref_id'";
				
		}
		$query="SELECT 
				pref_id, 
				pref_value 
			FROM 
				jorge_pref 
			WHERE 
				owner_id='".$this->user_id."'
				$sql
			AND
				vhost = '".$this->vhost."'
		";

		if ($pref_id === null) {

				$this->select($query,"raw");
				return $this->commit_select(array("pref_id","pref_value"));
				
			}
			else{

				return $this->select($query);
				
		}

		return false;

	}

	public function set_jorge_pref($pref_id,$pref_value) {

		$this->id_query = "Q062";
		$this->vital_check();
		$pref_id = $this->sql_validate($pref_id,"integer");
		$pref_value = $this->sql_validate($pref_value,"integer");
		if ($this->row_count("SELECT pref_id FROM jorge_pref WHERE owner_id='".$this->user_id."' AND pref_id='$pref_id' AND vhost='".$this->vhost."'") === false) {

			return false;

		}
		if ($this->result > 0) {

				$query="UPDATE
						jorge_pref 
					SET 
						pref_value='$pref_value' 
					WHERE 
						owner_id='".$this->user_id."' 
					AND 
						pref_id='$pref_id'
					AND
						vhost = '".$this->vhost."'
				";
				return $this->update($query);

			}
			else{

				$query="INSERT INTO 
						jorge_pref(owner_id,pref_id,pref_value,vhost) 
					VALUES 
						('".$this->user_id."','$pref_id','$pref_value','".$this->vhost."')
				";
				return $this->insert($query);
				
		}

		return false;

	}

	public function sphinx_get_single($peer_name_id, $peer_server_id, $peer_resource_id, $timestamp, $tslice) {

		$this->id_query = "Q063";
		$this->vital_check();
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		$peer_resource_id = $this->sql_validate($peer_resource_id,"integer");
		$timestamp = $this->sql_validate($timestamp,"string");
		$query="SELECT
				body
			FROM
				`".$this->construct_table($this->tslice)."`
			WHERE
				owner_id = '".$this->user_id."'
			AND
				peer_name_id = '".$this->peer_name_id."'
			AND
				peer_server_id = '".$this->peer_server_id."'
			AND
				peer_resource_id = '".$peer_resource_id."'
			AND
				timestamp like '".$timestamp."%'
				
		";

		return $this->select($query);
		
	}

	public function get_favorites() {

		$this->id_query = "Q064";
		$this->vital_check();
		$query="SELECT * 
			FROM 
				jorge_favorites 
			WHERE 
				owner_id='".$this->user_id."'
			AND
				vhost='".$this->vhost."'
			AND 
				ext is NULL 
			ORDER BY 
				str_to_date(tslice,'%Y-%m-%d')
			DESC
		";

		$this->select($query,"raw");
		return $this->commit_select(array("link_id","peer_name_id","peer_server_id","resource_id","tslice","comment"));
	
	}

	public function set_favorites($peer_name_id,$peer_server_id,$peer_resource_id = null, $tslice,$comment) {

		$this->id_query = "Q065";
		$this->vital_check();
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		#$peer_resource_id = $this->sql_validate($peer_resource_id,"integer");
		$comment = $this->sql_validate($comment,"string");
		$query="INSERT INTO 
				jorge_favorites(owner_id,peer_name_id,peer_server_id,tslice,comment,vhost) 
			VALUES(
				'".$this->user_id."',
				'".$this->peer_name_id."',
				'".$this->peer_server_id."',
				'".$this->tslice."',
				'$comment',
				'".$this->vhost."'
				)
			";
		
		return $this->insert($query);

	}

	public function delete_favorites_id($link_id) {

		$this->id_query = "Q066";
		$this->vital_check();
		$link_id = $this->sql_validate($link_id,"string");
		$query="DELETE FROM
				jorge_favorites
			WHERE
				owner_id = ".$this->user_id."
			AND
				vhost='".$this->vhost."'
			AND
				link_id = '$link_id';
		";
		
		return $this->delete($query);

	}

	public function check_favorite($peer_name_id,$peer_server_id,$tslice) {

		$this->id_query = "Q067";
		$this->vital_check();
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		$query="SELECT 
				count(*) as cnt
			FROM 
				jorge_favorites 
			WHERE 
				owner_id='".$this->user_id."' 
			AND 
				tslice='".$this->tslice."' 
			AND 
				peer_name_id='".$this->peer_name_id."' 
			AND 
				peer_server_id='".$this->peer_server_id."'
			AND
				vhost='".$this->vhost."'
			AND
				ext is null
		";

		return $this->select($query);

	}

	public function get_favorites_count() {

		$this->id_query = "Q068";
		$this->vital_check();
		$query="SELECT
				count(*) as cnt
			FROM
				jorge_favorites
			WHERE
				owner_id = '".$this->user_id."'
			AND
				vhost='".$this->vhost."'
			AND
				ext is null
		";

		return $this->select($query);

	}

	public function get_top_ten($date,$vhost = null) {

		$this->id_query = "Q069";
		if ($vhost === null) {

				$vh = $this->xmpp_host;

			}
			else{
				
				$vh = $this->vh($vhost,true);

		}
		$date = $this->sql_validate($date,"date");
		$query="SELECT
				at, 
				owner_id, 
				peer_name_id, 
				peer_server_id, 
				count 
			FROM 
				`logdb_stats_".$vh."` 
			WHERE 
				at = '$date' 
			ORDER BY 
				count 
			DESC LIMIT 10
		";
		
		$this->select($query,"raw");
		return $this->commit_select(array("at","owner_id","peer_name_id","peer_server_id","count"));

	}

	public function get_monthly_stats($vhost = null) {

		$this->id_query = "Q070";
		if ($vhost === null) {

				$vh = $this->xmpp_host;

			}
			else{
				
				$vh = $this->vh($vhost,true);

		}
		// This query need tweak to use ex.: where at between '2009-2' and '2009-3', it speeds up and corrects query, also forces to use index, instead full table scan
		$query="SELECT 
				count(distinct(owner_id)) AS users_total, 
				unix_timestamp(at)*10000 AS time_unix,
				sum(count) AS messages 
			FROM 
				`logdb_stats_".$vh."` 
			GROUP BY 
				at 
			ORDER BY 
				str_to_date(at,'%Y-%m-%d') 
			DESC LIMIT 30
		";
		
		$this->select($query,"raw");
		return $this->commit_select(array("users_total","time_unix","messages"));

	}

	public function get_hourly_stats($date,$vhost = null) {

		$this->id_query = "Q071";
		if ($vhost === null) {

				$vh = $this->vhost;

			}
			else{
				
				$vh = $this->vh($vhost);

		}
		$date = $this->sql_validate($date,"date");
		$query="SELECT
				hour,
				value 
			FROM 
				jorge_stats 
			WHERE 
				day='$date' 
			AND 
				vhost='".$vh."' 
			ORDER BY 
				hour 
			ASC
		";

		$this->select($query,"raw");
		return $this->commit_select(array("hour","value"));

	}

	public function get_weekly_stats($date_start,$date_end,$vhost = null) {

		$this->id_query = "Q072";
		if ($vhost === null) {

				$vh = $this->vhost;

			}
			else{
				
				$vh = $this->sql_validate($vhost,"string");

		}
		$date_start = $this->sql_validate($date_start,"date");
		$date_end = $this->sql_validate($date_end,"date");
		$query="SELECT 
				hour,
				value 
			FROM 
				jorge_stats 
			WHERE 
				day<='$date_end' 
			AND 
				day >= '$date_start' 
			AND 
				vhost='".$vh."' 
			ORDER BY 
				day,hour 
			ASC
		";

		$this->select($query,"raw");
		return $this->commit_select(array("hour","value"));

	}

	public function get_personal_top() {

		$this->id_query = "Q073";
		$this->vital_check();
		$query="SELECT 
				peer_name_id,
				peer_server_id,
				at,
				count 
			FROM 
				`logdb_stats_".$this->xmpp_host."` 
			WHERE 
				owner_id='".$this->user_id."' 
			AND
				peer_name_id!='".$this->ignore_id."' 
			AND 
				ext is NULL 
			ORDER BY 
				count 
			DESC LIMIT 10
		";
		
		$this->select($query,"raw");
		return $this->commit_select(array("peer_name_id","peer_server_id","at","count"));
	
	}

	public function get_personal_sum() {

		$this->id_query = "Q074";
		$this->vital_check();
		$query="SELECT
				sum(count) as cnt 
			FROM
				`logdb_stats_".$this->xmpp_host."` 
			WHERE 
				owner_id='".$this->user_id."' 
			AND
				peer_name_id!='".$this->ignore_id."'
		";
		return $this->select($query);

	}

	public function erase_all() {

		$this->id_query = "Q075";
		$this->vital_check();
		$query="SELECT 
				distinct(at) 
			FROM 
				`logdb_stats_".$this->xmpp_host."` 
			WHERE 
				owner_id='".$this->user_id."'
		";
		$this->select($query,"raw");
		$this->commit_select(array("at"));
		$results = $this->result;
		$this->id_query = "Q076";
		foreach ($results as $result) {

			$query="DELETE FROM 
					`logdb_messages_$result[at]_".$this->xmpp_host."` 
				WHERE 
					owner_id='".$this->user_id."'
				";
			if ($this->delete($query) === false) {
					
					return false;

			}
		
		}
		$this->id_query = "Q077";
		$query="DELETE FROM
				`logdb_stats_".$this->xmpp_host."` 
			WHERE 
				owner_id='".$this->user_id."'
			";
		if ($this->delete($query) === false) {

			return false;
			
		}
		if ($this->jorge_cleanup_soft() === false) {

			return false;

		}

		return true;

	}

	public function jorge_cleanup() {

		$this->id_query = "Q081";
		$this->vital_check();
		$query="DELETE FROM 
				jorge_pref 
			WHERE 
				owner_id='".$this->user_id."'
			AND
				vhost='".$this->vhost."'
			";
		if ($this->delete($query) === false) {

			return false;

		}
		$this->id_query = "Q082";
		$query="DELETE FROM
				`logdb_settings_".$this->xmpp_host."` 
			where 
				owner_id='".$this->user_id."'
			";
		if ($this->delete($query) === false) {

			return false;

		}

		return true;

	}

	public function jorge_cleanup_soft() {

		$this->id_query = "Q083";
		$this->vital_check();
		$query="DELETE FROM 
				jorge_mylinks 
			WHERE 
				owner_id='".$this->user_id."'
			AND
				vhost='".$this->vhost."'
			";
		if ($this->delete($query) === false) {

			return false;

		}
		$this->id_query = "Q084";
		$query="DELETE FROM 
				jorge_favorites 
			WHERE
				owner_id='".$this->user_id."'
			AND
				vhost='".$this->vhost."'
			";
		if ($this->delete($query) === false) {

			return false;

		}
		$this->id_query = "Q085";
		$query="DELETE FROM 
				pending_del 
			WHERE 
				owner_id='".$this->user_id."'
			AND
				vhost='".$this->vhost."'
			";
		if ($this->delete($query) === false) {

			return false;
		
		}

		return true;

	}

	public function get_next_prev_day($peer_name_id, $peer_server_id, $tslice, $np) {
	
		$this->id_query = "Q086";
		$this->vital_check();
		$this->prepare($peer_name_id,$peer_server_id,$tslice);
		if ($np === "n") {

				$sql1 = ">";
				$sql2 = "ASC";
			}
			elseif($np === "p") {

				$sql1 = "<";
				$sql2 = "DESC";
			}
			else{

				return false;

		}
		$query="SELECT
				at 
			FROM
				`logdb_stats_".$this->xmpp_host."`
			WHERE 
				owner_id='".$this->user_id."' 
			AND
				peer_name_id = '".$this->peer_name_id."' 
			AND 
				peer_server_id = '".$this->peer_server_id."' 
			AND 
				str_to_date(at, '%Y-%m-%d') $sql1 str_to_date('".$this->tslice."', '%Y-%m-%d') 
			ORDER BY 
				str_to_date(at,'%Y-%m-%d') 
			$sql2 LIMIT 1
		";
		
		return $this->select($query);

	}

	public function get_last_day() {

		$this->id_query = "Q087";
		$this->vital_check();
		if ($this->spec_ignore === true) {

			$sql = "AND peer_name_id != '".$this->ignore_id."'";

		}

		$query="SELECT 
				at 
			FROM
				`logdb_stats_".$this->xmpp_host."` 
			WHERE 
				owner_id = '".$this->user_id."' 
				$sql
			ORDER BY str_to_date(at,'%Y-%m-%d') DESC LIMIT 1
		";

		return $this->select($query);

	}

	private function get_ignore_id() {

		$this->id_query = "Q088";
		$query="SELECT
				user_id AS ignore_id
			FROM
				`logdb_users_".$this->xmpp_host."` 
			WHERE
				username=''
		";

		return $this->select($query);
		
	}

	private function set_ignore_id() {

		if ($this->get_ignore_id() === false) {

				return false;

			}
			else{

				$this->ignore_id = $this->result->ignore_id;
				return true;
				
		}

	}

	private function get_ext_index($table) {

		$this->id_query = "Q089";
		$query="SELECT
				ext+1 as idx 
			FROM 
				`".$table."` 
			WHERE
				owner_id = '".$this->user_id."'
			AND
				peer_name_id = '".$this->peer_name_id."'
			AND
				peer_server_id = '".$this->peer_server_id."'
			AND
				ext is not NULL
			ORDER BY
				ext
			DESC LIMIT 1
		";

		return $this->select($query);

	}

	public function get_last_attempt($user_id) {

		$this->id_query = "Q090";
		$user_id = $this->sql_validate($user_id,"integer");
		$query="SELECT
				count(id_user) AS cnt
			FROM
				jorge_logger 
			WHERE
				id_user = '$user_id' 
			AND
				log_time > date_sub(now(),interval 1 minute)
		";

		return $this->select($query);

	}

	public function is_left_or_right($date) {

		$this->id_query = "Q091";
		$query="SELECT
				at 
			FROM
				`logdb_stats_".$this->xmpp_host."` 
			WHERE
				owner_id='".$this->user_id."' 
			AND
				at like '$date%' 
			LIMIT 1
		";

		return $this->row_count($query);

	}

	public function set_own_name($own_name) {

		$this->id_query = "Q092";
		$this->vital_check();
		$own_name = $this->sql_validate($own_name,"string");
		$query="REPLACE INTO 
				jorge_self_names (owner_id, own_name, vhost) 
			VALUES
				(
				'".$this->user_id."',
				'".$own_name."',
				'".$this->vhost."'
				)
		";

		return $this->replace_q($query);

	}

	public function get_own_name() {

		$this->id_query = "Q093";
		$this->vital_check();
		$query="SELECT
				own_name 
			FROM
				jorge_self_names 
			WHERE
				owner_id='".$this->user_id."' 
			AND
				vhost='".$this->vhost."'
		";
		
		return $this->select($query);

	}

	public function set_ext_index($idx) {

		/*
			This is bit messy as we use ctype_digit() to check if string is an integer,
			so any number need to be set as type string and then pass to sql_validate() for validation.
			This should be changed in sql_validate() function to take care of all data that it gets...
		*/

		settype($idx,"string");
		if ($this->sql_validate($idx,"integer") === false) {

				return false;

			}
			else{

				$this->ext_idx = $idx;
				return true;

		}

		return false;

	}

	public function get_last_idx() {

		return $this->ext_idx;

	}

	public function set_user_query($user_query) {

		$this->user_query = $this->sql_validate($user_query,"string");
		return true;
		
	}

	public function remove_messages_from_trash($peer_name_id,$peer_server_id,$tslice) {

		if ($this->delete_messages($peer_name_id,$peer_server_id,$tslice) === true) {

				$this->unset_undo_table($peer_name_id,$peer_server_id,$tslice);
				$this->delete_mylinks($peer_name_id,$peer_server_id,$tslice);
				$this->delete_favorites($peer_name_id,$peer_server_id,$tslice);

			}

			else{

				return false;

		}

		return true;

	}

	public function db_error() {

		return $this->is_error;

	}

	public function set_user_id($user_id) {

		$user_id = $this->sql_validate($user_id,"integer");
		if ($user_id === false) {
				
				return false;

			}
			else {

				$this->user_id = $user_id;
				return true;
		}

		return false;
		

	}

	protected function sql_validate($val,$type) {

		if($this->db_driver === "mysql") {

			if ($type==="integer") {

				// Default - all this "integer" strings are really characters with should be numeric. Need Test!
				settype($val,"string");

				if(ctype_digit($val)) {
					
						return $val;

					}
				else{
						$this->is_error = true;
						return false;
				}
			}
			elseif($type==="string") {

				return mysql_escape_string($val);

				}

			elseif($type==="date") {

				list($ye, $mo, $da) = split("-", $val);
				if (!ctype_digit($ye) || !ctype_digit($mo) || !ctype_digit($da)) { 
		
						$this->is_error = true;
						return false;
						
					} 
					else { 
					
						return $val;
						
				}

				$this->is_error = true;
				return false;
			}
			else{
				$this->is_error = true;
				return false;
			}


		}

	return false;

	}

	private function commit_select($arr) {

		if ($this->is_error === true) {
				
				return false;

			}

		$this->object_to_array($arr);
		return true;
	
	}

	private function object_to_array($arr) {
		
		settype($i, "integer");
		settype($z, "integer");
		$result = $this->result;
		while($row = mysql_fetch_object($result)) {
			
			$i++;
			foreach ($arr as $key) {
				
				$z++;
				$items[$i][$key] = $row->$key;
		
			}


		}

		return $this->result = $items;
		
	}

	private function prepare($peer_name_id = null,$peer_server_id = null, $tslice = null) {

		if ($peer_name_id !== null) {

				$this->peer_name_id = $this->sql_validate($peer_name_id,"integer");

		}

		if ($peer_server_id !== null) {

				$this->peer_server_id = $this->sql_validate($peer_server_id,"integer");

		}

		if ($tslice !== null) {

				$this->tslice = $this->sql_validate($tslice,"date");

		}

		$this->vital_check();
		return;

	}

	private function construct_table($tslice) {
		
		return $tslice_table = $this->messages_table.''.$tslice.'_'.$this->xmpp_host;

	}

	private function vital_check() {

		if($this->user_id === false OR !$this->user_id) {

				print "<br><br><small>Operation aborted! Can't continue.</small><br><br>";
				exit; // abort all, user_id MUST be set.
		}
		return true;

	}

	private function vh($vhost,$dash = null) {

		if ($dash === true) {

				return str_replace(".","_",$this->sql_validate($vhost,"string"));


			}
			else{
				
				return $this->sql_validate($vhost,"string");

		}

	}

	public function set_debug($bool) {

		if($bool === true) { 
		
				$this->is_debug = true; 
				return true;
				
			}
			elseif($bool === false) {

				$this->is_debug = false;
				return true;

		}

		return false;

	}

	public function spec_ignore($bool) {

		if($bool === true) {

				$this->spec_ignore = true;
				return true;

			}
			elseif($bool === false) {

				$this->spec_ignore = false;
				return true;

		}

		return false;

	}

	private function show_debug_info($query = null, $time = null) {

		if ($this->is_debug === true) {
		
			if ($query !== null) {
					
					print "<br><small><b>QueryID:</b> ".$this->id_query.": ".htmlspecialchars($query)."<br>";
				}
			if ($query === null AND $time !== null) {
			
					print "<b>SQL performed in:</b> ".$this->time_result."</small><br><br>";

				}
		
		}
	}

	private function sql_time() {
		
		list($usec, $sec) = explode(" ",microtime());
		return ((float)$usec + (float)$sec);
	}

	private function time_start() {

		if ($this->is_debug === true) {
		
			return $this->time_start = $this->sql_time();
		
		}
	
	}

	private function time_end() {

		if ($this->is_debug === true) {
		
			$start = $this->time_start;
			$end = $this->sql_time();
			return $this->time_result = substr($end - $start, 0, 10);

		}

	}

	public function __destruct() {

		mysql_free_result();
		mysql_close();
		$this->user_id = null;
		$this->result = null;

	}

}

?>
