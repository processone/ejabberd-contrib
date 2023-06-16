mod_captcha_rust - Generate CAPTCHAs using Rust library
=======================================================

Requires:
- ejabberd 23.01 or higher compiled with Elixir
- Erlang/OTP and Elixir to compile the `captcha_nif` Elixir library
- Rust compiler (package `rustc` in some package managers)


This small module generates CAPTCHA images suitable for ejabberd's
[CAPTCHA](https://docs.ejabberd.im/admin/configuration/basic/#captcha) feature,
thanks to the [captcha](https://github.com/feng19/captcha) Elixir library,
which internally uses the [captcha](https://github.com/daniel-e/captcha) Rust library.

See example CAPTCHA images in the
[captcha Rust documentation](https://docs.rs/captcha/latest/captcha/).


Get `captcha_nif` library
-------------------------

This module depends on an Elixir library, and there are two ways to get it installed,
depending on how you obtained ejabberd:

### A) ejabberd source code

If you compile ejabberd from source code, go to the path with your
ejabberd source code and apply this small patch:
```diff
--- a/mix.exs
+++ b/mix.exs
@@ -99,6 +99,7 @@ defmodule Ejabberd.MixProject do
 
   defp deps do
     [{:base64url, "~> 1.0"},
+     {:captcha_nif, "~> 0.1", hex: :captcha_nif},
      {:cache_tab, "~> 1.0"},
      {:eimp, "~> 1.0"},
      {:ex_doc, ">= 0.0.0", only: :dev},
```

Then get the new dependencies, and compile as usual:
```
mix deps.get
make
```

Install ejabberd as usual, restart it
and now you can proceed to install `mod_captcha_rust`.


### B) ejabberd with binary installer

If you installed ejabberd using some kind of binary installer,
you can download, compile and install the required library manually:

```
git clone https://github.com/feng19/captcha.git
cd captcha/
mix deps.get
mix
mix release
```

Copy all the directories from `_build/dev/rel/captcha/lib`
to the path where you have ejabberd lib directories installed.

Now you can restart ejabberd and proceed to install `mod_captcha_rust`.


Basic Configuration
-------------------

The minimal configuration required to get this module working is:

```yaml
captcha_cmd: 'Elixir.ModCaptchaRust'

captcha_url: http://localhost:5280/captcha

listen:
  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      /captcha: ejabberd_captcha
```

Options
-------

This module supports those configurable options:

* `difficulty`

  Sets the CAPTCHA difficulty, it can be `easy`, `medium` or `hard`.
  Default value: `easy`.

* `color`

  Sets the CAPTCHA color, as a list of RGB integers from 0 to 255.
  Default value: `[0, 0, 0]`.


Example of module configuration:

```yaml
modules:
  'Elixir.ModCaptchaRust':
    difficulty: hard
    color: [255, 0, 0]
```
