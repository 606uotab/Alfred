defmodule Alfred.Voice do
  @moduledoc """
  Voix d'Alfred — synthèse vocale via espeak-ng.
  Alfred peut parler à voix haute pour les notifications et rappels.
  """

  alias Alfred.Storage.Local, as: Storage

  @config_file "voice_config.json"
  @default_config %{
    "enabled" => false,
    "lang" => "fr",
    "rate" => "160",
    "volume" => "100"
  }

  @doc "Parle à voix haute (non-bloquant)."
  def speak(text) when is_binary(text) do
    if enabled?() and available?() do
      config = load_config()
      lang = config["lang"] || "fr"
      rate = config["rate"] || "160"
      volume = config["volume"] || "100"

      Task.start(fn ->
        try do
          System.cmd("espeak-ng", [
            "-v", lang,
            "-s", to_string(rate),
            "-a", to_string(volume),
            text
          ], stderr_to_stdout: true)
        rescue
          _ -> :ok
        end
      end)

      :ok
    else
      {:error, :disabled}
    end
  end

  @doc "Vérifie si espeak-ng est installé."
  def available? do
    case System.find_executable("espeak-ng") do
      nil -> false
      _ -> true
    end
  end

  @doc "Vérifie si la voix est activée."
  def enabled? do
    config = load_config()
    config["enabled"] == true
  end

  @doc "Active la voix."
  def enable do
    config = load_config()
    save_config(Map.put(config, "enabled", true))
    :ok
  end

  @doc "Désactive la voix."
  def disable do
    config = load_config()
    save_config(Map.put(config, "enabled", false))
    :ok
  end

  @doc "Configure la voix."
  def configure(opts) when is_map(opts) do
    config = load_config()
    updated = Map.merge(config, opts)
    save_config(updated)
    :ok
  end

  @doc "Retourne le statut de la voix."
  def status do
    config = load_config()
    %{
      enabled: config["enabled"] == true,
      available: available?(),
      lang: config["lang"] || "fr",
      rate: config["rate"] || "160"
    }
  end

  defp load_config do
    case Storage.read(@config_file) do
      data when is_map(data) and map_size(data) > 0 -> data
      _ -> @default_config
    end
  end

  defp save_config(config) do
    Storage.write(@config_file, config)
  end
end
