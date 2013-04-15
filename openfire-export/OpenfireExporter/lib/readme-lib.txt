
Copy in this directory the following files:

commons-fileupload-1.0.jar
dom4j-1.6.1.jar
isorelax.jar
msv.jar
relaxngDatatype.jar
xsdlib.jar

You can get those files from the original Openfire plugin: userImportExport.jar

Note: openfire.jar file in this folder must be used if you want to export the
private stored info also you will need to comment out lines 332 up to 335 in 
ExporterXEP227.java class and finally recompile it.
Have fun !

---
