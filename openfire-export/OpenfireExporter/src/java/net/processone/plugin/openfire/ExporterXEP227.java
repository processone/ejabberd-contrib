package net.processone.plugin.openfire;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.fileupload.FileItem;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.dom4j.io.OutputFormat;
import org.dom4j.io.SAXReader;
import org.dom4j.io.XMLWriter;
import org.jivesoftware.openfire.OfflineMessage;
import org.jivesoftware.openfire.OfflineMessageStore;
import org.jivesoftware.openfire.PrivateStorage;
import org.jivesoftware.openfire.XMPPServer;
import org.jivesoftware.openfire.auth.AuthFactory;
import org.jivesoftware.openfire.container.Plugin;
import org.jivesoftware.openfire.container.PluginManager;
import org.jivesoftware.openfire.roster.RosterItem;
import org.jivesoftware.openfire.roster.RosterItemProvider;
import org.jivesoftware.openfire.user.User;
import org.jivesoftware.openfire.user.UserAlreadyExistsException;
import org.jivesoftware.openfire.user.UserManager;
import org.jivesoftware.openfire.user.UserNotFoundException;
import org.jivesoftware.openfire.user.UserProvider;
import org.jivesoftware.openfire.vcard.VCardManager;
import org.jivesoftware.stringprep.Stringprep;
import org.jivesoftware.stringprep.StringprepException;
import org.jivesoftware.util.Log;
import org.xmpp.packet.JID;

/**
 * <p>
 * The user import/export plugin provides a way to import and export Openfire
 * user data via the Admin Console. This plugin is XEP-0227 compliant
 * </p>
 * This plugin can export: <li>User data</li> <li>vCard</li> <li>Offline
 * messages</li> <br/>
 * <p>
 * <b>See also:</b> {@link List http://xmpp.org/extensions/xep-0227.html}
 * </p>
 * 
 * @author <a href="mailto:smartinez@process-one.net">Vidal Santiago
 *         Martinez</a>
 */
public class ExporterXEP227 implements Plugin {
	
	private static final String LOAD_ALL_PRIVATE = "SELECT privateData FROM ofPrivate WHERE username=?";
	
	private UserManager userManager;
	private UserProvider provider;
	private String serverName;
	private OfflineMessageStore offlineMessagesStore;
	private VCardManager vCardManager;
	private PrivateStorage privateStorage;

	
	public ExporterXEP227() {
		userManager = XMPPServer.getInstance().getUserManager();
		offlineMessagesStore = XMPPServer.getInstance()
				.getOfflineMessageStore();
		provider = UserManager.getUserProvider();
		serverName = XMPPServer.getInstance().getServerInfo().getXMPPDomain();
		privateStorage = XMPPServer.getInstance().getPrivateStorage();
		vCardManager = VCardManager.getInstance();
	}

	public void initializePlugin(PluginManager manager, File pluginDirectory) {
	}

	public void destroyPlugin() {
		userManager = null;
		provider = null;
		serverName = null;
		offlineMessagesStore = null;
	}

	/**
	 * Convenience method that returns true if this UserProvider is read-only.
	 * 
	 * @return true if the user provider is read-only.
	 */
	public boolean isUserProviderReadOnly() {
		return provider.isReadOnly();
	}

	/**
	 * Converts the user data that is to be exported to a byte[]. If a read-only
	 * user store is being used a user's password will be the same as their
	 * username.
	 * 
	 * @return a byte[] of the user data.
	 * @throws IOException
	 *             if there's a problem writing to the XMLWriter.
	 */
	public byte[] exportUsersToByteArray() throws IOException {
		ByteArrayOutputStream out = new ByteArrayOutputStream();

		XMLWriter writer = new XMLWriter(out, OutputFormat.createPrettyPrint());
		writer.write(exportUsers());

		return out.toByteArray();
	}

	/**
	 * Converts the exported user data to a String. If a read-only user store is
	 * being used a user's password will be the same as their username.
	 * 
	 * @return a formatted String representation of the user data.
	 * @throws IOException
	 *             if there's a problem writing to the XMLWriter.
	 */
	public String exportUsersToString() throws IOException {
		StringWriter stringWriter = new StringWriter();
		XMLWriter writer = null;
		try {
			writer = new XMLWriter(stringWriter, OutputFormat
					.createPrettyPrint());
			writer.write(exportUsers());
		} catch (IOException ioe) {
			Log.error(ioe);
			throw ioe;
		} finally {
			if (writer != null) {
				writer.close();
			}
		}

		return stringWriter.toString();
	}

	/**
	 * Returns a list of usernames that were unable to be imported or whose
	 * rosters could not imported. Users are not able to be imported for the
	 * following reasons: <li>Their username is not properly formatted. <li>If a
	 * read-only user data store is being used and the user could not be found.
	 * <li>If a writeable user data store is being used and the user already
	 * exists.
	 * 
	 * @param file
	 *            a FileItem containing the user data to be imported.
	 * @param previousDomain
	 *            a String an optional parameter that if supplied will replace
	 *            the user roster entries domain names to server name of current
	 *            Openfire installation.
	 * @return True if FileItem matches the openfire user schema.
	 * @throws IOException
	 *             if there is a problem reading the FileItem.
	 * @throws DocumentException
	 *             if an error occurs during parsing.
	 */
	public List<String> importUserData(FileItem file, String previousDomain)
			throws DocumentException, IOException {
		SAXReader reader = new SAXReader();
		Document document = reader.read(file.getInputStream());
		return importUsers(document, previousDomain);
	}

	/**
	 * Returns whether or not the supplied FileItem matches the openfire user
	 * schema
	 * 
	 * @param file
	 *            a FileItem to be validated.
	 * @return True if FileItem matches the openfire user schema.
	 */
	public boolean validateImportFile(FileItem file) {
		try {
			return new UserSchemaValidator(file, "wildfire-user-schema.xsd")
					.validate();
		} catch (Exception e) {
			Log.error(e);
			return false;
		}
	}

	/**
	 * Adding heading of an user and its parameters
	 * 
	 * @param userElement
	 *            DOM element
	 * @param user
	 *            User object
	 */
	private void addUser(Element userElement, User user) {

		String userName = user.getUsername();
		userElement.addAttribute("name", userName);

		try {
			userElement.addAttribute("password", AuthFactory
					.getPassword(userName));

		} catch (UserNotFoundException e) {
			Log.info("User " + userName
					+ " not found, setting their password to their username");
			userElement.addAttribute("password", userName);
		} catch (UnsupportedOperationException e) {
			Log.info("Unable to retrieve " + userName
					+ " password, setting their password to their username");
			userElement.addAttribute("password", userName);
		}

	}

	/**
	 * Add roster and its groups to a DOM element
	 * 
	 * @param userElement
	 *            DOM element
	 * @param user
	 *            User
	 */
	private void addRoster(Element userElement, User user) {
		Element rosterElement = userElement.addElement("query",
				"jabber:iq:roster");

		Collection<RosterItem> roster = user.getRoster().getRosterItems();
		for (RosterItem ri : roster) {
			Element itemElement = rosterElement.addElement("item");

			itemElement.addAttribute("jid", ri.getJid().toBareJID());
			itemElement.addAttribute("name", ri.getNickname());
			itemElement.addAttribute("subscription", ri.getSubStatus()
					.getName());

			/**
			 * Adding groups
			 */
			Element groupElement = itemElement.addElement("group");
			List<String> groups = ri.getGroups();
			for (String group : groups) {
				groupElement.addText(group);
			}

		}
	}

	/**
	 * Adding offline messages, if there are some.
	 * 
	 * @param hostname
	 *            host name
	 * @param userElement
	 *            DOM element
	 * @param userName
	 *            user name
	 */
	private void addOfflineMessages(String hostname, Element userElement,
			String userName) {
		Collection<OfflineMessage> offlineMessages = offlineMessagesStore
				.getMessages(userName, false);

		if (!offlineMessages.isEmpty()) {
			Element offlineElement = userElement.addElement("offline-messages");

			for (OfflineMessage offMessage : offlineMessages) {
				Element messageElement = offlineElement.addElement("message",
						"jabber:client");
				messageElement.addAttribute("from", offMessage.getFrom()
						.toString());
				messageElement
						.addAttribute("to", offMessage.getTo().toString());
				messageElement
						.addAttribute("type", offMessage.getType().name());

				/**
				 * Adding text message
				 */
				Element bodyElement = messageElement.addElement("body");
				String body = offMessage.getBody();
				bodyElement.addText(body != null ? body : "");

				/**
				 * Adding delay element
				 */
				Element delayElement = messageElement.addElement("delay",
						"urn:xmpp:delay");
				delayElement.addAttribute("from", hostname);
				delayElement.addAttribute("stamp", offMessage.getCreationDate()
						.toString());
				delayElement.addText("Offline Storage");
			}

		}

	}

	/**
	 * Adding vcard element
	 * 
	 * @param userElement
	 *            DOM element
	 * @param userName
	 *            user name
	 */
	@SuppressWarnings("unchecked")
	private void addVCard(Element userElement, String userName) {
		Element vCard = vCardManager.getVCard(userName);
		if (vCard != null) {
			Element vCardElement = userElement
					.addElement("vCard", "vcard-temp");
			for (Iterator<Element> iterator = vCard.elementIterator(); iterator
					.hasNext();) {
				Element element = iterator.next();
				vCardElement.addElement(element.getName()).addText(
						element.getText());
			}
		}
	}

	/**
	 * Add all the private stored information (XEP-0049)
	 * <b>Note: this method is not suported in the available openfire releases,
     * If you want to use it, you will need to change your openfire.jar file to
     * openfire.jar file in this projects lib folder, remove comment to this
     * method and recompile the plugin.
     *  
	 * </b>  
	 * @param userName User name
	 * @param userElement User element
	 */
	private void addPrivateStorage(String userName, Element userElement) {
//		Element result = privateStorage.getAll(userName);
//		if (result.elements().size() > 0) {
//			userElement.add(result.createCopy());
//		}
	}

	/**
	 * Export the user list and its, Vcard, Offline mesages and roster list to
	 * an XML representation XEP-0227 compliant
	 * 
	 * @return DOM document
	 */
	private Document exportUsers() {
		Document document = DocumentHelper.createDocument();

		Element root = document.addElement("server-data",
				"http://www.xmpp.org/extensions/xep-0227.html#ns");

		Element host = root.addElement("host");

		host.addAttribute("jid", serverName);

		Collection<User> users = userManager.getUsers();
		for (User user : users) {

			String userName = user.getUsername();

			Element userElement = host.addElement("user");

			addUser(userElement, user);

			addRoster(userElement, user);

			addVCard(userElement, userName);

			addOfflineMessages(serverName, userElement, userName);

			addPrivateStorage(userName, userElement);

		}

		return document;
	}

	private List<String> importUsers(Document document, String previousDomain) {
		List<String> invalidUsers = new ArrayList<String>();

		UserManager userManager = UserManager.getInstance();
		RosterItemProvider rosterItemProvider = RosterItemProvider
				.getInstance();

		Element users = document.getRootElement();

		Iterator usersIter = users.elementIterator("User");
		while (usersIter.hasNext()) {
			Element user = (Element) usersIter.next();

			String userName = null;
			String password = null;
			String email = null;
			String name = null;
			List<RosterItem> rosterItems = new ArrayList<RosterItem>();

			Iterator userElements = user.elementIterator();
			while (userElements.hasNext()) {
				Element userElement = (Element) userElements.next();

				String nameElement = userElement.getName();
				if ("Username".equals(nameElement)) {
					userName = userElement.getText();
				} else if ("Password".equals(nameElement)) {
					password = userElement.getText();
				} else if ("Name".equals(nameElement)) {
					name = userElement.getText();
				} else if ("Email".equals(nameElement)) {
					email = userElement.getText();
				} else if ("Roster".equals(nameElement)) {
					Iterator rosterIter = userElement.elementIterator("Item");

					while (rosterIter.hasNext()) {
						Element rosterElement = (Element) rosterIter.next();

						String jid = rosterElement.attributeValue("jid");
						String askstatus = rosterElement
								.attributeValue("askstatus");
						String recvstatus = rosterElement
								.attributeValue("recvstatus");
						String substatus = rosterElement
								.attributeValue("substatus");
						String nickname = rosterElement.attributeValue("name");

						List<String> groups = new ArrayList<String>();
						Iterator groupIter = rosterElement
								.elementIterator("Group");
						while (groupIter.hasNext()) {
							Element group = (Element) groupIter.next();
							groups.add(group.getText());
						}

						// used for migration
						if (previousDomain != null) {
							jid = jid.replace(previousDomain, serverName);
						}

						rosterItems.add(new RosterItem(new JID(jid),
								RosterItem.SubType.getTypeFromInt(Integer
										.parseInt(substatus)),
								RosterItem.AskType.getTypeFromInt(Integer
										.parseInt(askstatus)),
								RosterItem.RecvType.getTypeFromInt(Integer
										.parseInt(recvstatus)), nickname,
								groups));
					}
				}
			}

			if ((userName != null) && (password != null)) {
				try {
					userName = Stringprep.nodeprep(userName);

					if (!isUserProviderReadOnly()) {
						userManager.createUser(userName, password, name, email);
					}

					// Check to see user exists before adding their roster, this
					// is for read-only user providers.
					userManager.getUser(userName);
					for (RosterItem ri : rosterItems) {
						rosterItemProvider.createItem(userName, ri);
					}
				} catch (StringprepException se) {
					Log.info("Invalid username " + userName);
					invalidUsers.add(userName);
				} catch (UserAlreadyExistsException e) {
					Log.info("User already exists " + userName);
					invalidUsers.add(userName);
				} catch (UserNotFoundException e) {
					Log.info("User not found " + userName);
					invalidUsers.add(userName);
				}
			}
		}

		return invalidUsers;
	}
}
