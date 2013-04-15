ejabberd-modules is a collaborative development area for ejabberd
modules developers and users.


    For users
    =========

You need to have Erlang installed.

To use an ejabberd module coming from this repository:

- Read the module specific README to see if special steps are needed
  to deploy it.

- Run "./build.sh" or "build.bat" in the root (usually trunk
  directory) of the wanted module.

- Copy generated .beam files from the ebin directory to the directory
  where your ejabberd .beam files are.

- Use the configuration file examples provided in the conf dir to
  update your ejabberd.cfg configuration file.

If during compilation of a module you get an error like:
  {"init terminating in do_boot",{undef,[{make,all,[]},...
it means Erlang couldn't find its file make.beam
In Debian and other distributions you can try to install packages like:
  erlang-dev erlang-nox erlang-tools


    For developers
    ==============

The following organisation has been set-up for the development:

- Each module has its own SVN structure (trunk/branches/tags) to allow
  independent versioning.

- Development and compilation of module should be possible without
  ejabberd SVN, as long as developers check-out the ejabberd-dev
  module. This module contains include file to make compilation
  possible.

- The module directory structure is usually the following:
 README.txt: Module description
 LICENSE.txt: License for the module
 Emakefile: Erlang makefile to build the module (preferred way, if no
   dependencies on C code, as build will thus works on Windows)
 doc/: Documentation dir
 src/: Source directory
 src/msgs/: Directory with translation files (pot, po and msg).
 ebin/: empty (Target directory for the build).
 conf/: Directory containing example configuration for your module.
 build.sh: *nix build script.
 build.bat: Windows build script.

- Module developers should put in the README if the module has
  requirements or known incompatibilities with other modules (for
  example, by modifying the same main ejabberd modules).

