defmodule Alfred do
  @moduledoc """
  Alfred — Votre majordome numérique dévoué.
  """

  @version "0.1.0"

  def version, do: @version

  def data_dir do
    Path.join(System.user_home!(), ".alfred/data")
  end
end
