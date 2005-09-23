## This file provides the integration of XML/Ada into GPS

import GPS

GPS.parse_xml ("""
   <doc_path>share/doc/xmlada</doc_path>
   <documentation_file>
      <name>xml.html</name>
      <descr>XML/Ada User's Guide</descr>
      <category>XMLAda</category>
      <menu before="About">/Help/XML Ada/XML Ada User's Guide</menu>
   </documentation_file>
""")
