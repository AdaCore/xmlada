.. _Using_the_library:

*****************
Using the library
*****************

XML/Ada is a library. When compiling an application that uses it, you
thus need to declare this dependency. The default installation implies the
use of GNAT project files. See the GPRbuild and GPR Companion Tools User's
Guide for more information on the project files and
how to create them for your application.

Basically, a project file contains the description of your build
environment (source directories, object directories, libraries, etc).

The very simple case is when you have all your sources in the same
directory (say :file:`src/`), and the object files are all generated in the
:file:`obj/` directory.

.. highlight:: ada

In this case, your project file would look like::

  with "xmlada";
  project Default is
    for Source_Dirs use ("src/");
    for Object_Dir use "obj/";
  end Default;

and you build your application with::

  gprbuild -Pdefault main.adb

Note in the project file the first line, which indicates that your
application requires XML/Ada to build. This will automatically set the
appropriate compiler and linker switches to use XML/Ada. Your application
will be linked against all modules of XML/Ada (DOM, SAX, ...).

If your application doesn't use DOM, you can replace the first line with
something like::

  with "xmlada_sax";

which will reduce the number of libraries that your application is
linked with.

If the installation prefix is the same as your GNAT installation (which is
the case of the preinstalled version of the library), then GPRbuild will
automatically find XML/Ada's project files. If XML/Ada is not installed into
a predefined location (e.g. because you rebuilt it from sources), you need to
let GPRbuild know where to find the project files. This is done by setting the
`GPR_PROJECT_PATH` environment variable, by adding to it the directory that
contains :file:`xmlada.gpr`.

Check the :file:`dom/test` directory in the XML/Ada package, which contains
both code examples and project files that you can use as a basis for your
own code.

The default type of library depends on the way you installed XML/Ada. In all
cases, and assuming you installed both static and shared libraries, you can
choose among the two by setting the environment variable::

  LIBRARY_TYPE=static

or::

  LIBRARY_TYPE=relocatable

Whatever method you used to build your application, you might have to change,
at least on UNIX systems, the environment variable `LD_LIBRARY_PATH` so that
it contains the :file:`lib/` directory in the XML/Ada installation, so that the
dynamic libraries are correctly found.

This is not needed if you build XML/Ada as a static library.

Running on VxWorks
==================

On VxWorks, XML Ada processing might require more stack space than what is
typically available from the VxWorks shell, the tasks spawned from there with
"sp", or Ada tasks with no or a too small Storage_Size value attached.

Such stack overflow conditions are typically characterized by non-deterministic
erratic behavior and can be cured by allocating more stack space for the tasks
involved.

