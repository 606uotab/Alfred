defmodule Alfred.Storage.Local do
  @moduledoc """
  Stockage local en fichiers JSON dans ~/.alfred/data/.
  """

  def ensure_data_dir! do
    dir = Alfred.data_dir()

    unless File.exists?(dir) do
      File.mkdir_p!(dir)
    end
  end

  def ensure_subdir!(subdir) do
    dir = Path.join(Alfred.data_dir(), subdir)

    unless File.exists?(dir) do
      File.mkdir_p!(dir)
    end

    dir
  end

  def read(filename) do
    path = filepath(filename)

    if File.exists?(path) do
      path
      |> File.read!()
      |> Jason.decode!()
    else
      []
    end
  end

  def write(filename, data) do
    path = filepath(filename)
    File.mkdir_p!(Path.dirname(path))
    json = Jason.encode!(data, pretty: true)
    File.write!(path, json)
  end

  defp filepath(filename) do
    Path.join(Alfred.data_dir(), filename)
  end
end
