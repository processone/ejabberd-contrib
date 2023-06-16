mod_ecaptcha - Generate CAPTCHAs using ecaptcha
===============================================

Requires:
- ejabberd 23.01 or higher
- C compiler and makefile
- write access to ejabberd's priv dir


This small module uses the
[ecaptcha](https://github.com/seriyps/ecaptcha)
erlang library to generate CAPTCHA images suitable for ejabberd's
[CAPTCHA](https://docs.ejabberd.im/admin/configuration/basic/#captcha) feature.

Installing this module in a containerized ejabberd requires
some additional steps, please read below.


Basic Configuration
-------------------

The minimal configuration required to get this module working is:

```yaml
captcha_cmd: mod_ecaptcha

captcha_url: http://localhost:5280/captcha

listen:
  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      /captcha: ejabberd_captcha

modules:
  mod_ecaptcha: {}
```


Options
-------

The configurable options match mostly the ones from ecaptcha library:

- `numchars`

  Number of characters to include in the CAPTCHA image.
  Default: `5`

- `effects`

  List of effects to use to generate the CAPTCHA image.
  Check [ecaptcha usage](https://github.com/seriyps/ecaptcha#usage)
  for details.
  Default: `[line, blur, filter, dots, reverse_dots]`

- `color`

  This option defines the image's color. Valid values:
  `black`, `red`, `orange`, `blue`, `pink` or `purple`.
  Default: `black`

- `alphabet`

  String containing all the characters that can be printed on the image
  (duplicates are ok).
  The default value includes: numbers, latin characters lower and upper case.

- `font`

  String of one of the supported fonts.
  Please notice that fonts are pre-rendered at NIF compile-time,
  see the `deps/ecaptcha/c_src/fonts.h` and `FONTS` parameter in `deps/ecaptcha/c_src/Makefile`.
  Default: `hplhs-oldstyle`


Example Configuration
---------------------

This example configuration setups CAPTCHA images that are simple to solve:
just 3 characters, only the vocals in lower and upper case.

It also setups `mod_register_web`, you can test the feature immediately
visiting `http://localhost:5280/register/`

```yaml
captcha_cmd: mod_ecaptcha

captcha_url: http://localhost:5280/captcha

listen:
  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      /captcha: ejabberd_captcha
      /register: mod_register_web

modules:
  mod_ecaptcha:
    numchars: 3
    effects: [line, dots]
    color: red
    alphabet: "aeiouAEIOU"
    font: "hplhs-oldstyle"
```


Install in Container
--------------------

There are some additional steps to install this module in a containerized ejabberd.

For example, if using the container image available at GitHub Container Registry:
```
docker run --name ejabberd -it -p 5222:5222 -p 5280:5280 -p 5288:5288 ghcr.io/processone/ejabberd:23.01 live
```

Install the dependencies required to compile C code:
```
docker exec --user root ejabberd apk add git make g++ freetype-dev erlang-dev
```

It's a good idea to update specs and modules source code:
```
docker exec ejabberd ejabberdctl modules_update_specs
```

Install the module so it gets the Erlang dependencies,
then compile that dependency C code, and finally recompile it:
```
docker exec ejabberd ejabberdctl module_install mod_ecaptcha
docker exec ejabberd make -C .ejabberd-modules/sources/ejabberd-contrib/mod_ecaptcha/deps/ecaptcha/c_src
docker exec ejabberd ejabberdctl module_upgrade mod_ecaptcha
```

If using the Docker Hub Container, `ejabberdctl` is in a different path,
please use those commands instead:
```
docker exec ejabberd bin/ejabberdctl modules_update_specs
docker exec ejabberd bin/ejabberdctl module_install mod_ecaptcha
docker exec ejabberd make -C .ejabberd-modules/sources/ejabberd-contrib/mod_ecaptcha/deps/ecaptcha/c_src
docker exec ejabberd bin/ejabberdctl module_upgrade mod_ecaptcha
```
