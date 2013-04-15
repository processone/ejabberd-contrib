<%@ page import="java.io.OutputStream,
                 org.jivesoftware.openfire.XMPPServer,
                 net.processone.plugin.openfire.ExporterXEP227"
         contentType="application/x-download" %>
         
<!--NOTE: This JSP file should be implemented with JSTL and Servlet,
	Is not a good idea to call your self pages and add scriplet to handle -->
	         
<%String fileName = request.getParameter("fileName");
    response.setContentType("application/x-download");
    response.setHeader("Content-Disposition","attachment;filename="+fileName+".xml");
    ExporterXEP227 plugin = (ExporterXEP227) XMPPServer.getInstance().getPluginManager().getPlugin("openfireexporter");
    byte[] content = plugin.exportUsersToByteArray();
    OutputStream os = response.getOutputStream();
    os.write(content);
    os.flush();
    os.close();%>