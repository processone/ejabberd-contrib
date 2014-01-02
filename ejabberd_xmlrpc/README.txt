
        ejabberd_xmlrpc - XML-RPC server

        Homepage: http://www.ejabberd.im/ejabberd_xmlrpc
        Author: Badlop


	DESCRIPTION
	-----------

ejabberd_xmlrpc is an ejabberd listener that starts a XML-RPC server
and waits for external calls.

ejabberd_xmlrpc implements some example calls that can be used to test
during the development of a new XML-RPC client.  But most
importantly, ejabberd_xmlrpc is also a frontend to execute ejabberd
commands.  This way a XML-RPC client can execute any ejabberd command.

This allows external programs written in any language like websites or
administrative tools to communicate with ejabberd to get information
or to make changes without the need to know ejabberd internals.  One
example usage is a corporate site in PHP that creates a Jabber user
every time a new user is created on the website.

Some benefits of interfacing with the Jabber server by XML-RPC instead
of modifying directly the database are:
 - external programs are more simple and easy to develop and debug
 - can communicate with a server in a different machine, and even on Internet


	REQUIREMENTS
	------------

    ejabberd 2.1.0 or higher
    XMLRPC-Erlang 1.13 with IP, Ruby and Xmerl 1.x patches
    Optional: mod_admin_extra implements many ejabberd commands for general server administration
    Optional: mod_muc_admin implements ejabberd commands for MUC administration


	CONFIGURE EJABBERD
	------------------

1. You need to get and install XMLRPC-Erlang.
You can download XMLRPC-Erlang binary files from
  http://www.ejabberd.im/ejabberd_xmlrpc
or compile it yourself:
  wget http://www.ejabberd.im/files/contributions/xmlrpc-1.13-ipr2.tgz
  tar -xzvf xmlrpc-1.13-ipr2.tgz
  cd xmlrpc-1.13/src
  make
  cd ../../
Then you can copy the *.beam files to ejabberd ebin directory,
or add an option like this to the ejabberd start script:
$ erl -pa '/home/jabber/xmlrpc-1.13/ebin' ...

2. Configure ejabberd to start this listener at startup:
edit ejabberd.cfg and add on the 'listen' section:
{listen, [
   {4560, ejabberd_xmlrpc, []},
    ...
 ]}.

3. Start ejabberd.

4. Verify that ejabberd is listening in that port:
$ netstat -n -l | grep 4560
tcp        0      0 0.0.0.0:4560            0.0.0.0:*               LISTEN

5. If there is any problem, check ejabberd.log and sasl.log files


	CONFIGURE
	---------

The listener allow several configurable options:

    {maxsessions, Integer}
    Number of concurrent connections allowed.
    Default: 10

    {timeout, Integer}
    Timeout of the connections, expressed in milliseconds.
    Default: 5000

    {access_commands, AccessCommands}
    This option allows to define a list of access restrictions.
    If this option is present, then XML-RPC calls must include as
    first argument a struct with a user, server and password of an
    account in ejabberd that has privileges in Access.
    If the option is not present, such struct must not be provided.
    The default value is to not define any restriction: []
    When one or several access restrictions are defined and the
    XML-RPC call provides authentication for an account, each
    restriction is verified until one matches completely:
    the account matches the Access rule,
    the command name is listed in CommandNames,
    and the provided arguments do not contradict Arguments.
    There is more information about AccessCommands in the ejabberd Guide.


Example configuration: XML-RPC calls can execute any command, with any
argument, and no authentication information must be provided:
{listen, [
  {4560, ejabberd_xmlrpc, [{maxsessions, 10}, {timeout, 5000}]},
  ...
 ]}.

In this case authentication information must be provided, but it is
enough that the account exists and the password is valid to execute
any command:
{listen, [
  {4560, ejabberd_xmlrpc, [{maxsessions, 10}, {timeout, 5000},
                           {access_commands, [{all, all, []}]}]},
  ...
 ]}.

In this example the local Jabber account xmlrpc-robot@jabber.example.org
can execute any command with no argument restriction:
{acl, xmlrpcbot, {user, "xmlrpc-robot", "jabber.example.org"}}.
{access, xmlrpcaccess, [{allow, xmlrpcbot}]}.
{listen, [
  {4560, ejabberd_xmlrpc, [{maxsessions, 10}, {timeout, 5000},
                           {access_commands, [{xmlrpcaccess, all, []}]}]},
  ...
 ]}.

Finally, in this complex example the listener only listens in port
4560 of IP address 127.0.0.1, and several access restrictions are
defined (the corresponding ACL and ACCESS are not shown):
{listen, [
  {{4560, "127.0.0.1"}, ejabberd_xmlrpc, [
    {access_commands, [
      %% This bot can execute any command:
      {xmlrpc_bot, all, []},
      %% This bot can execute any command,
      %% but if a 'host' argument is provided, it must be "example.org":
      {xmlrpc_bot_all_example, all, [{host, "example.org"}]},
      %% This bot can only execute the command 'dump'. No argument restriction:
      {xmlrpc_bot_backups, [dump], []}
      %% This bot can only execute the command 'register',
      %% and if argument 'host' is provided, it must be "example.org":
      {xmlrpc_bot_reg_example, [register], [{host, "example.org"}]},
      %% This bot can execute the commands 'register' and 'unregister',
      %% if argument host is provided, it must be "test.org":
      {xmlrpc_bot_reg_test, [register, unregister], [{host, "test.org"}]}
    ]}
  ]},
  ...
 ]}.

Example configuration in ejabberd 13, using YAML formatting:
acl:
  restxmlrpcadmin:
    user:
      - "admin": "example.com"
  xmlrpcadmin:
    user:
      - "adminjabber": "example.org"
  rpcadmin:
    user:
      - "admin1": "example.net"

access:
  restxmlrpcaccess:
    restxmlrpcadmin: allow
    admin: allow
  xmlrpcaccess:
    xmlrpcadmin: allow
  rpcaccess:
    rpcadmin: allow

listen:
-
    #ip: "127.0.0.1"
    port: 4560
    module: ejabberd_xmlrpc
    timeout: 30000
    maxsessions: 20
    access_commands:
            restxmlrpcaccess:
                all : []
            xmlrpcaccess:
                commands: [register,status]
                options: []
            rpcaccess:
                commands: [unregister,status]
                options: []


	USAGE
	-----

You can send calls to http://host:4560/

Call:           Arguments:                                             Returns:

 -- debug
echothis        String                                                   String
echothisnew     struct[{sentence, String}]           struct[{repeated, String}]
multhis         struct[{a, Integer}, {b, Integer}]                      Integer
multhisnew      struct[{a, Integer}, {b, Integer}]        struct[{mu, Integer}]

 -- statistics
tellme_title    String                                                   String
tellme_value    String                                                   String
tellme          String                 struct[{title, String}, {value. String}]


With ejabberd_xmlrpc you can execute any ejabberd command with a XML-RPC call.

1. Get a list of available ejabberd commands, for example:
$ ejabberdctl help
Available commands in this ejabberd node:
  connected_users              List all established sessions
  connected_users_number       Get the number of established sessions
  delete_expired_messages      Delete expired offline messages from database
  delete_old_messages days     Delete offline messages older than DAYS
  dump file                    Dump the database to text file
  register user host password  Register a user
  registered_users host        List all registered users in HOST
  reopen_log                   Reopen the log files
  restart                      Restart ejabberd
  restore file                 Restore the database from backup file
  status                       Get ejabberd status
  stop                         Stop ejabberd
  unregister user host         Unregister a user
  user_resources user host     List user's connected resources

2. When you found the command you want to call, get some additional
   help of the arguments and result:
$ ejabberdctl help user_resources
  Command Name: user_resources
  Arguments: user::string
             host::string
  Returns: resources::[ resource::string ]
  Tags: session
  Description: List user's connected resources

3. You can try to execute the command in the shell for the account testuser@localhost:
$ ejabberdctl user_resources testuser localhost
Home
Psi

4. Now implement the proper XML-RPC call in your XML-RPC client.
   This example will use the Erlang library:
$ erl
1> xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, user_resources, [{struct, [{user, "testuser"}, {host, "localhost"}]}]}).
{ok,{response,[{struct,[{resources,{array,[{struct,[{resource,"Home"}]},
                                           {struct,[{resource,"Psi"}]}]}}]}]}}

5. Note: if ejabberd_xmlrpc has the option 'access_commands'
   configured with some access restriction (see the example
   configurations provided above), the XML-RPC must include first an
   argument providing information of a valid account. For example:
1> xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, user_resources, [
   {struct, [{user, "adminuser"}, {server, "localhost"}, {password, "aeiou"}]},
   {struct, [{user, "testuser"}, {host, "localhost"}]} ]}).


Arguments in XML-RPC calls can be provided in any order;
This module will sort the arguments before calling the ejabberd command.

If auth is provided in the call when ejabberd_xmlrpc does not require it,
the call will return the error: -112 Unknown call



	EXAMPLE IN PHP
	--------------

This is an XML-RPC client in PHP, thanks to Zbyszek Żółkiewski and Calder.
It requires "allow_url_fopen = On" in your php.ini.

-------
<?
$param=array("user"=>"testuser", "host"=>"localhost");
$request = xmlrpc_encode_request('user_resources', $param, (array('encoding' => 'utf-8')));

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

The response, following the example would be like this:
-------
$ php5 call.php
Array
(
    [resources] => Array
        (
            [0] => Array
                (
                    [resource] => Home
                )
            [1] => Array
                (
                    [resource] => Psi
                )
        )
)
-------

If you configured the option access_commands, you have to provide authentication
information by replacing the first lime with something like this:
-------
$param_auth=array("user"=>"analloweduser", "server"=>"localhost", "password"=>"MyPasS997");
$param_comm=array("user"=>"testuser", "host"=>"localhost");
$param=array($param_auth, $param_comm);
-------


 **** WARNING: all the remaining text was written for mod_xmlrpc and
      is NOT valid for ejabberd_xmlrpc ****


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

4> xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, register,
[{struct, [{user, "ggeo"}, {host, "example.com"}, {password, "gogo11"}]}]}).
{ok,{response,[0]}}

5> xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, register,
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

result = server.register(params)
print result
-------

This Python example shows how to provide authentication in the call, thanks to Muelli:
-------
import xmlrpclib

server_url = 'http://127.0.0.1:4560'
server = xmlrpclib.ServerProxy(server_url)

EJABBERD_XMLRPC_LOGIN = {'user': 'adminuser', 'server': 'localhost', 'password': 'aeiou'}

def ejabberdctl(command, data):
    fn = getattr(server, command)
    return fn(EJABBERD_XMLRPC_LOGIN, data)

result = ejabberdctl('register', {'user':'ggeo', 'host':'localhost', 'password':'gogo11'})
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


	EXAMPLE IN C#
	-------------

This is an XML-RPC client in C#, thanks to Mitchel Constantin.

-------
// Copyright: 2010 Weavver, Inc. 
// Author: Mitchel Constantin <mythicalbox@weavver.com>
// License: Public Domain (Limited to this file)

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CookComputing.XmlRpc;

namespace Weavver.Vendors.ProcessOne
{
     public struct send_message_chat
     {
          public string from;
          public string to;
          public string body;
     }
     public struct status {}
     public class ejabberdRPC
     {
          [XmlRpcUrl("http://205.134.225.18:4560/RPC2")]
          public interface IStateName : IXmlRpcProxy
          {
               [XmlRpcMethod("send_message_chat")]
               object SendMessageChat(send_message_chat message);
               [XmlRpcMethod("status")]
               object Status(status s);
          }
          public CookComputing.XmlRpc.XmlRpcStruct SendMessageChat(send_message_chat message)
          {
               IStateName proxy = XmlRpcProxyGen.Create<IStateName>();
               proxy.KeepAlive = false;
               return (CookComputing.XmlRpc.XmlRpcStruct) proxy.SendMessageChat(message);
          }
          public CookComputing.XmlRpc.XmlRpcStruct Status(status status)
          {
               IStateName proxy = XmlRpcProxyGen.Create<IStateName>();
               proxy.KeepAlive = false;
               return (CookComputing.XmlRpc.XmlRpcStruct) proxy.Status(status);
          }
     }
}
-------
