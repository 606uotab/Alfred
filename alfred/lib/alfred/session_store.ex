defmodule Alfred.SessionStore do
  @moduledoc """
  Session chiffrée persistante — stocke le token Mistral et les credentials
  dans un fichier chiffré avec une clé dérivée du mot de passe du coffre.
  Utilise AES-256-GCM via :crypto (Erlang OTP 27).
  """

  @session_file Path.expand("~/.alfred/session.enc")
  @salt_size 16
  @iv_size 12
  @tag_size 16
  @kdf_iterations 10_000
  @default_ttl_hours 24

  # -- API publique --

  @doc "Sauvegarde une session chiffrée sur disque."
  def save(password, token, soul, culture, opts \\ []) do
    ttl_seconds = cond do
      Keyword.has_key?(opts, :ttl) -> Keyword.get(opts, :ttl)
      true -> Keyword.get(opts, :ttl_hours, @default_ttl_hours) * 3600
    end
    expires_at = DateTime.utc_now() |> DateTime.add(ttl_seconds, :second) |> DateTime.to_iso8601()

    payload = Jason.encode!(%{
      "token" => token,
      "soul" => soul,
      "culture" => culture,
      "expires_at" => expires_at,
      "created_at" => DateTime.utc_now() |> DateTime.to_iso8601()
    })

    salt = :crypto.strong_rand_bytes(@salt_size)
    key = derive_key(password, salt)
    iv = :crypto.strong_rand_bytes(@iv_size)

    {ciphertext, tag} = :crypto.crypto_one_time_aead(
      :aes_256_gcm, key, iv, payload, <<>>, @tag_size, true
    )

    blob = salt <> iv <> tag <> ciphertext

    File.mkdir_p!(Path.dirname(@session_file))
    File.write!(@session_file, blob)
    File.chmod!(@session_file, 0o600)
    :ok
  rescue
    e ->
      Alfred.Log.error("SessionStore", "Erreur sauvegarde: #{Exception.message(e)}")
      {:error, Exception.message(e)}
  end

  @doc "Charge la session chiffrée depuis le disque."
  def load(password) do
    case File.read(@session_file) do
      {:ok, blob} when byte_size(blob) > @salt_size + @iv_size + @tag_size ->
        <<salt::binary-size(@salt_size),
          iv::binary-size(@iv_size),
          tag::binary-size(@tag_size),
          ciphertext::binary>> = blob

        key = derive_key(password, salt)

        case :crypto.crypto_one_time_aead(
          :aes_256_gcm, key, iv, ciphertext, <<>>, tag, false
        ) do
          plaintext when is_binary(plaintext) ->
            data = Jason.decode!(plaintext)
            check_expiration(data)

          :error ->
            {:error, "Déchiffrement échoué (mauvais mot de passe?)"}
        end

      {:ok, _} ->
        {:error, "Fichier session corrompu"}

      {:error, :enoent} ->
        {:error, :no_session}

      {:error, reason} ->
        {:error, "Lecture impossible: #{inspect(reason)}"}
    end
  rescue
    e ->
      {:error, "Erreur chargement session: #{Exception.message(e)}"}
  end

  @doc "Supprime le fichier de session."
  def clear do
    File.rm(@session_file)
    :ok
  end

  @doc "Vérifie si un fichier session existe."
  def exists? do
    File.exists?(@session_file)
  end

  # -- Private --

  defp derive_key(password, salt) do
    initial = :crypto.mac(:hmac, :sha256, salt, password)

    Enum.reduce(1..@kdf_iterations, initial, fn _, acc ->
      :crypto.mac(:hmac, :sha256, salt, acc)
    end)
  end

  defp check_expiration(data) do
    case DateTime.from_iso8601(data["expires_at"] || "") do
      {:ok, expires, _} ->
        if DateTime.compare(DateTime.utc_now(), expires) == :lt do
          {:ok, %{
            token: data["token"],
            soul: data["soul"],
            culture: data["culture"]
          }}
        else
          {:error, :expired}
        end

      _ ->
        {:error, "Date d'expiration invalide"}
    end
  end
end
