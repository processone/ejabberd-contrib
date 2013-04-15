
        mod_xmlrpc - XML-RPC server

        Homepage: http://www.ejabberd.im/mod_xmlrpc
        Author: Badlop 

	Note: this module will only receive bugfixes.
	If you are using ejabberd trunk SVN 1635 or newer, you will probably
	prefer to install ejabberd_xmlrpc.
	

	DESCRIPTION
	-----------

mod_xmlrpc is a module for ejabberd, a XMPP/Jabber server written in Erlang.
It starts a XML-RPC server and waits for external requests. Implemented calls
include statistics and user administration. This allows external programs written
in any language like websites or administrative tools to communicate with 
ejabberd to get information or to make changes without the need to know ejabberd 
internals.
    
One example usage is a corporate site in PHP that creates a Jabber user every time
a new user is created on the website.

Some benefits of interfacing with the Jabber server by XML-RPC instead of modifying
directly the database are:
 - external programs are more simple and easy to develop and debug
 - can communicate with a server in a different machine, and even on Internet


	REQUIREMENTS
	------------

    ejabberd 1.1.2 or newer
    XMLRPC-Erlang 1.13 with IP, Ruby and Xmerl 1.x patches
    Optional: mod_muc_admin for MUC-related calls
    Optional: ejabberd SVN trunk r1329 or newer is required for muc_room_set_affiliation


 - Install XMLRPC-Erlang

wget http://ejabberd.jabber.ru/files/contributions/xmlrpc-1.13-ipr2.tgz
tar -xzvf xmlrpc-1.13-ipr2.tgz
cd xmlrpc-1.13/src
make
cd ../../


	
	CONFIGURE EJABBERD
	------------------

1. Add an option like this to the ejabberd start script:

$ erl -pa '/home/jabber/xmlrpc-1.13/ebin' ...

2. Configure ejabberd to start this module at startup: 
edit ejabberd.cfg and add on 'modules' section:

	{mod_xmlrpc,      []},

3. Now start ejabberd. If everything went ok, no mention to mod_xmlrpc 
will be shown on the error log.


	CONFIGURE
	---------

Configuration is done in 'modules' section in ejabberd.cfg. Available options:
    port: port where the XML-RPC server will listen. Default: 4560
    ip: IP address to listen, in Erlang format. Set to 'all' to listen on all IP address. Default: all
    maxsessions: Number of concurrent connections allowed. Default: 10
    timeout: Timeout of the connections, expressed in milliseconds. Default: 5000

Example configurations:
    {mod_xmlrpc,     [{port, 4560}]}

    {mod_xmlrpc,     [{port, 4560}, {ip, all}, {maxsessions, 10}, {timeout, 5000}]}

    {mod_xmlrpc,     [{port, 4560}, {ip, {81, 202, 202, 79}}]}



	USAGE
	-----

You can send calls to http://host:4560/ 

Call:           Arguments:                                                 Returns:

 -- debug
echothis        String                                                       String
multhis         struct[{a, Integer}, {b, Integer}]                          Integer

 -- statistics
tellme_title    String                                                       String
tellme_value    String                                                       String
tellme          String                     struct[{title, String}, {value. String}]

 -- user administration
create_account  struct[{user, String}, {host, String}, {password, String}]  Integer
delete_account  struct[{user, String}, {host, String}, {password, String}]  Integer
check_account   struct[{user, String}, {host, String}]                      Integer
check_password  struct[{user, String}, {host, String}, {password, String}]  Integer
change_password struct[{user, String}, {host, String}, {newpass, String}]   Integer
num_resources   struct[{user, String}, {host, String}]                      Integer
resource_num    struct[{user, String}, {host, String}, {num, Integer}]       String
set_nickname    struct[{user, String}, {host, String}, {nickname, String}]  Integer
add_rosteritem  struct[{localuser, String}, {localserver, String}, 
                       {user, String}, {server, String}, 
                       {nick, String}, {group, String}, {subs, String}]      String
delete_rosteritem  struct[{localuser, String}, {localserver, String},
                          {user, String}, {server, String}]                  String
get_roster  struct[{user, String}, {server, String}]
                      array[struct[{jid, String}, {nick, String}, {Group, String}]]
send_message  struct[{from, String}, {to, String}, {subject, String}, 
                      {body, String}]                                       Integer

 -- MUC administration
create_muc_room struct[{name, String}, {service, String}, {server, String}] Integer
delete_muc_room struct[{name, String}, {service, String}, {server, String}] Integer
muc_room_change_option struct[{name, String}, {service, String},
                              {option, String}, {value, String}]            Integer
muc_room_set_affiliation struct[{name, String}, {service, String},
                                {jid, String}, {affiliation, Affiliation}]  Integer


	TEST
	----

 - You can easily try the XML-RPC server starting a new Erlang Virtual Machine 
   and making calls to ejabberd's XML-RPC:

1. Start Erlang with this option:
$ erl -pa '/home/jabber/xmlrpc-1.13/ebin'

2. Now on the Erlang console, write commands and check the results:

1> xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, echothis, [800]}).
{ok,{response,[800]}}

2> xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, echothis, ["blot cloc 557.889 kg"]}).
{ok,{response,["blot cloc 557.889 kg"]}}

3> xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, multhis, [{struct,[{a, 83}, {b, 689}]}]}).
{ok,{response,[57187]}}

4> xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, create_account, 
[{struct, [{user, "ggeo"}, {host, "example.com"}, {password, "gogo11"}]}]}).
{ok,{response,[0]}}

5> xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, create_account, 
[{struct, [{user, "ggeo"}, {host, "example.com"}, {password, "gogo11"}]}]}).
{ok,{response,[409]}}

6> xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, muc_room_change_option,
[{struct, [{name, "test"}, {service, "conference.localhost"},
 {option, "title"}, {value, "Test Room"}]}]}).

7> xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, muc_room_set_affiliation, 
[{struct, [{name, "test"}, {service, "conference.example.com"}, 
{jid, "ex@example.com"}, {affiliation, "member"}]}]}).

8> xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, muc_room_set_affiliation, 
[{struct, [{name, "test"}, {service, "conference.example.com"}, 
{jid, "ex@example.com"}, {affiliation, "none"}]}]}).


 - Some possible XML-RPC error messages:

   + Client: connection refused: wrong IP, wrong port, the server is not started...

2> xmlrpc:call({127, 0, 0, 1}, 44444, "/", {call, echothis, [800]}).
{error,econnrefused}

   + Client: bad value: a800 is a string, so it must be put into ""

7> xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, echothis, [a800]}).
{error,{bad_value,a800}}

   + Server: unknown call: you sent a call that the server does not implement 

3> xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, bububu, [800]}).
{ok,{response,{fault,-1,"Unknown call: {call,bububu,[800]}"}}}


	EXAMPLE IN PYTHON
	-----------------

This is an example XML-RPC client in Python, thanks to Diddek:
-------
import xmlrpclib

server_url = 'http://127.0.0.1:4560';
server = xmlrpclib.Server(server_url);

params = {}
params["user"] = "ggeo"
params["host"] = "localhost"
params["password"] = "gogo11"

result = server.create_account(params)
print result
-------


	EXAMPLE IN RUBY
	---------------

This is an example XML-RPC client in Ruby, thanks to Diddek:
-------
require 'xmlrpc/client'

host = "172.16.29.6:4560"
timeout = 3000000
client = XMLRPC::Client.new2("http://#{host}", "#{host}", timeout)
result = client.call("echothis", "800")
puts result 
-------


	EXAMPLE IN PHP
	--------------

This is an XML-RPC client in PHP, thanks to Zbyszek Żółkiewski and Calder.
It requires "allow_url_fopen = On" in your php.ini.

-------
<?
$param=array("user"=>"test_user","host"=>"example.com","password"=>"some_password");
$request = xmlrpc_encode_request('check_password', $param, (array('encoding' => 'utf-8')));

$context = stream_context_create(array('http' => array(
    'method' => "POST",
    'header' => "User-Agent: XMLRPC::Client mod_xmlrpc\r\n" .
                "Content-Type: text/xml\r\n" .
                "Content-Length: ".strlen($request),
    'content' => $request
)));

$file = file_get_contents("http://127.0.0.1:4560/RPC2", false, $context);

$response = xmlrpc_decode($file);

if (xmlrpc_is_fault($response)) {
    trigger_error("xmlrpc: $response[faultString] ($response[faultCode])");
} else {
    print_r($response);
}

?>
-------


	EXAMPLE IN JAVA
	---------------

This is an XML-RPC client in Java, thanks to Calder.
It requires Apache XML-RPC available at http://ws.apache.org/xmlrpc/

-------
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.apache.xmlrpc.client.XmlRpcClient;
import org.apache.xmlrpc.client.XmlRpcClientConfigImpl;

public class Test {

	public static void main(String[] args) {
		try {
		    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
		    config.setServerURL(new URL("http://127.0.0.1:4560/RPC2"));
		    XmlRpcClient client = new XmlRpcClient();
		    client.setConfig(config);

		    /* Command string */
		    String command = "check_password";

		    /* Parameters as struct */
		    Map struct = new HashMap();
		    struct.put("user", "test1");
		    struct.put("host", "localhost");
		    struct.put("password", "test");

		    Object[] params = new Object[]{struct};
		    Integer result = (Integer) client.execute(command, params);
		    System.out.println(result);
		} catch (Exception e) {
			System.out.println(e);
		}
	}

}
-------
