defmodule Ejabberd.Module.CaptchaRust do

  @behaviour :gen_mod
  import Ejabberd.Logger

  require Record

  ##====================================================================
  ## gen_mod callbacks
  ##====================================================================

  def start(_host, _opts) do
    :ok
  end

  def stop(_host) do
    :ok
  end

  def depends(_host, _opts) do
    []
  end

  def mod_opt_type(:color) do
    :econf.list(:econf.int(0, 255))
  end

  def mod_opt_type(:difficulty) do
    :econf.enum([:easy, :medium, :hard])
  end

  def mod_options(_host) do
    [
     {:color, [0, 0, 0]},
     {:difficulty, :easy},
    ]
  end

  def mod_doc() do
    %{:desc => 'This is just a demonstration.'}
  end

  ##====================================================================
  ## Captcha Rust
  ##====================================================================

  def create_image(_key) do
    [r, g, b] = get_opt(:color)
    color = %{r: r, g: g, b: b}

    {captcha_text, captcha_image} = case get_opt(:difficulty) do
      :easy -> Captcha.easy(set_color: color)
      :medium -> Captcha.medium(set_color: color)
      :hard -> Captcha.hard(set_color: color)
    end

    {:ok, "image/png", captcha_text, captcha_image}
  end

  def get_opt(option_name) do
    host = List.first(:ejabberd_option.hosts())
    :gen_mod.get_module_opt(host, Ejabberd.Module.CaptchaRust, option_name)
  end

end
