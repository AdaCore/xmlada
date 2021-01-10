XML/Ada: An XML parser for Ada95
================================

INSTALLING THIS LIBRARY ON UNIX
-------------------------------

Make sure that you do not have a previous installation of XML/Ada in one
of the directories given in ADA_OBJECTS_PATH, or some files will not be
properly recompiled.

Compiling with GNAT 3.16 or newer versions of GNAT
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You need to automatically detect a few aspects of your build environment,
which is done by running
    ./configure --prefix=PREFIX
where PREFIX is the installation directory you wish to use. It is recommended
to install into the GNAT directory. This way the project files will be
installed into the default location where gnatmake and gprbuild are looking
for projects.

If, however, you are running a cross-compiler, you need to add an additional
command line switch to configure, namely
    ./configure --prefix=PREFIX --target=TARGET
where target will be similar to "powerpc-wrs-vxworks", "powerpc-elinos-linux",
or any other prefix used to find the name of the gnatmake executable to run.

By default, XML/Ada will build both static and shared libraries, if
"configure" detected that the latter were supported on your system. You can
prevent the building of shared libraries by specifying --disable-shared as
an extra switch to configure.

Once installed, your applications will by default be linked statically with
XML/Ada. You can change that default by specifying explicitly --enable-shared
as an extra switch to configure.

To install the library, use the following command line:
   make all install

If your application is built using a runtime which is not the default,
you need to build XmlAda with the same runtime. This is done by simply
adding an RTS=NAME argument to the make command above. For instance,
to build and install the XmlAda library with the "rtp" runtime, adjust
the make command above as follow:

   make all install RTS=rtp

You can also override the default install location by adding the argument
IPREFIX=PATH to the make command above.

Compiling with other Ada compilers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This library has been reported as being compilable with other compilers than
GNAT. No build script is provided however.

INSTALLING THIS LIBRARY ON WINDOWS
----------------------------------

1. Due to the nature of GNU Make you need to have a UNIX-type shell and
   utilities installed to build this library. If you do not have this,
   you can download a set at:

     Cygwin : http://cygwin.com/
     Mingw  : http://www.mingw.org/ (MSYS Toolset)

2. Run 'sh configure --prefix=<installation directory>' to generate the
   makefile.
   where "installation directory" is the place where you want to install
   XML/Ada.

3. Before building make sure to set the following values in 'Makefile':
      PREFIX=C:/GNATPRO/6.4.2
   the makefile where to install the library. Note the slashes in the path!
   The value should have been set properly by the call to configure.

   It is recommended to install into the GNAT directory. This way the project
   files will be installed into the default location where gnatmake and
   gprbuild are looking for projects.

4. As a general rule for GNAT, the temporary directory must be accessible
   from gnatmake native applications (not a cygwin path). For instance, from
   cygwin:
       export TMPDIR=c:/tmp
   Failing to check this might prevent the building of shared libraries

5. The best way to proceed is to do a 'make all install'.
   It is possible that make complains that it cannot execute an "install"
   executable. In such a case, it is recommended to execute
   "make INSTALL=cp install" instead to use "cp" to do the installation

TWEAKING THE LIBRARY
--------------------

You can change the way the library behaves, in particular the DOM part,
by changing some of the hard-coded constants in the code. If you change
these, you will need to recompile the library and your application:

- sax-encodings.ads :: Encoding
  It contains the encoding used internally by the library. The default is
  UTF8, which supports the whole Unicode range, and is economical
  memory-wise

- dom-core.ads :: Shared_Strings  and dom-core.ads :: Shared_Node_Names
  Whether the internal representation of namespaces should be shared among
  nodes. The default is yes

USING THE LIBRARY
-----------------

See the XML/Ada user's guide for information on how to use this library.

TESTING THE LIBRARY
-------------------

Several test programs are provided in the XML/Ada source package. These
are found in the dom/test and sax/test subdirectories. These are very simple
examples on how to use the DOM and SAX API.

The w3c organization has constructed an official testsuite that can be used to
check XML/Ada. Most likely, this is only useful to you if you are modifying
XML/Ada itself. Here is how you can run this testsuite:
   You need to download the official XML Conformance Testsuite for XML,
   The name of the current archive is
       xmlts20080827.tar
   The URL is:
       http://www.w3.org/XML/Test/

   Uncompress it somewhere on your disk, then add a link in tests/dom, which
   points to the directory where you unpackaged the testsuite. The link should
   be called "tests".

A similar setup is required for the W3C schema testsuite. The URL of the
testsuite is:
   http://www.w3.org/XML/2004/xml-schema-test-suite/xmlschema2006-11-06/xsts-2007-06-20.tar.gz
This file should be extracted in the tests/schema directory (creating a
xmlschema-2006-11-06 subdirectory as a result).

   Then execute the command from the toplevel directory:
       make run_test

   This will report the number of errors found.

The W3C committee has reviewed the status of a number of tests since the
testsuite was released (as developers of XML parsers have reported issues
with the testsuite). They provide a public CVS repository that include the
metadata for the tests (list of tests, their status, expected output,...),
but unfortunately not the tests themselves. As a result, getting the latest
version requires several steps. First get the .tar.gz as described above,
and extract it in schema/test. From the same directory, run:

   cvs -d :pserver:anonymous@dev.w3.org:/sources/public login
   cvs -d :pserver:anonymous@dev.w3.org:/sources/public -z5 co XML/xml-schema-test-suite/2004-01-14/xmlschema2006-11-06

You can update that directory at any time with:
   cd XML; cvs update

The XML/Ada testsuite will automatically take advantage of that directory
if found. You need however to manually run
   cd schema/test
   ./schematest --cvs
to get the output under those conditions, since "make run_test" is setup
for the .tar.gz output.


CONTENTS OF THE LIBRARY
-----------------------

The sources in this library are split into several subdirectories, each
with its own README, sources, documentation and unit tests.

The list of subdirectories (aka modules) is:

- unicode:
  provides a full support for Unicode and Utf8/Utf16/Utf32 encodings. It also
  support other encodings like Latin1, Latin2, ...

- input_sources
  provides types to read characters from several types of media.

- sax
  Provides a common, standard interface for any XML parser, through
  callback subprograms. You can extend any of the tagged types defined in these
  packages and thus parse XML files without having to store an XML tree in
  memory.
  This package is compatible with SAX 2.0, but doesn't implement the functions
  specific to SAX 1.0

- dom
  The Document Object Model is a set of standard subprograms to manipulate
  XML tree in memory.
  Only the Core module is currently implemented.

- docs
  This contains the full documentation for this XML library

BUG REPORTS
-----------

Please send questions and bug reports to report@gnat.com following
the same procedures used to submit reports with the GNAT toolset itself.
