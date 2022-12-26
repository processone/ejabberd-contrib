mod_ecaptcha - Generate CAPTCHAs using ecaptcha
===============================================

Requires:
- ejabberd 23.xx or higher
- C compiler and makefile
- write access to ejabberd's priv dir


This small module uses the
[ecaptcha](https://github.com/seriyps/ecaptcha)
erlang library to generate CAPTCHA images suitable for ejabberd's
[CAPTCHA](https://docs.ejabberd.im/admin/configuration/basic/#captcha) feature.


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

