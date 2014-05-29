ejabberd-contrib
================

This is a collaborative development area for ejabberd module developers
and users.


For users
---------

To use an ejabberd module coming from this repository:

- You need to have Erlang installed.

- Read the module-specific `README.txt` file to see if special steps are
  required to deploy it.

- Run `./build.sh` or `build.bat` in the root directory of the desired
  module.

- Copy generated `.beam` files from the `ebin` directory to the directory
  where your ejabberd `.beam` files are.

- Use the configuration file examples provided in the `conf` dir to update
  your `ejabberd.cfg` or `ejabberd.yml` configuration file.

If during compilation of a module you get an error like:

    {"init terminating in do_boot",{undef,[{make,all,[]},...

it means Erlang couldn't find its `make.beam` file.  In Debian and other
distributions you can try to install packages like:

    erlang-dev erlang-nox erlang-tools


For developers
--------------

The following organization has been set up for the development:

- Development and compilation of modules should be possible without the
  ejabberd source code, as the `ejabberd-dev` helper module contains the
  include files necessary to make compilation possible.

- The module directory structure is usually the following:
    * `README.txt`: Module description.
    * `LICENSE.txt`: License for the module.
    * `Emakefile`: Erlang Makefile to build the module (preferred way, if
      no dependencies on C code, as build will thus work on Windows).
    * `doc/`: Documentation directory.
    * `src/`: Source directory.
    * `src/msgs/`: Directory with translation files (pot, po and msg).
    * `ebin/`: Empty (target directory for the build).
    * `conf/`: Directory containing example configuration for your module.
    * `build.sh`: Unix/Linux build script.
    * `build.bat`: Windows build script.

- Module developers should note in the `README.txt` file whether the
  module has requirements or known incompatibilities with other modules
  (for example, by modifying the same main ejabberd modules).
