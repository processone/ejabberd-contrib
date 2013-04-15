<%@ page import="net.processone.plugin.openfire.ExporterXEP227,
                 org.jivesoftware.openfire.XMPPServer"
%>

<!--NOTE: This JSP file should be implemented with JSTL and Servlet,
	Is not a good idea to call your self pages and add scriplet to handle -->
	
<html>
    <head>
        <title>Import/Export Selection</title>
        <meta name="pageID" content="import-export-selection"/>
    </head>
    <body>

<jsp:useBean id="pageinfo" scope="request" class="org.jivesoftware.admin.AdminPageBean" />
<%
	ExporterXEP227 plugin = (ExporterXEP227) XMPPServer.getInstance().getPluginManager().getPlugin("openfireexporter");
%>
<p>
Openfire plugin improved to export to XEP-227 format.
There is also old code to import, but using a custom Openfire format.
If you are interested you can improve it to also support XEP-227 import. 

<ul>
    <li><a href="import-user-data.jsp">Import User Data</a></li>
    <li><a href="export-user-data.jsp">Export User Data</a></li>    
</ul>

<% 
if (plugin.isUserProviderReadOnly()) { 
%>

   Note: because you are using a read-only user data store such as LDAP or POP3 you will only be able to import user roster data, not users themselves.
   Please see the <a href="../../plugin-admin.jsp?plugin=openfireexporter&showReadme=true&decorator=none">readme</a> for details.

<%
} 
%>
</p>

</body>
</html>
