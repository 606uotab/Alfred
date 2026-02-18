defmodule Alfred.Colors do
  @moduledoc """
  Couleurs ANSI pour le terminal — le style visuel d'Alfred.
  """

  def bold(text),    do: "\e[1m#{text}\e[0m"
  def dim(text),     do: "\e[2m#{text}\e[0m"
  def green(text),   do: "\e[32m#{text}\e[0m"
  def red(text),     do: "\e[31m#{text}\e[0m"
  def yellow(text),  do: "\e[33m#{text}\e[0m"
  def cyan(text),    do: "\e[36m#{text}\e[0m"
  def blue(text),    do: "\e[34m#{text}\e[0m"

  def icon_ok,   do: green("✓")
  def icon_warn, do: yellow("⚠")
  def icon_err,  do: red("✗")
  def icon_down, do: red("✗")

  def header(text), do: "  ── #{cyan(bold(text))} ──"
end
