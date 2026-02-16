defmodule Alfred.Remind.Commands do
  @moduledoc """
  Commandes de rappels — Alfred veille sur le temps de son maître.
  Les rappels sont gérés par le gen_server Erlang alfred_scheduler.
  """

  alias Alfred.Butler

  def handle([project | rest]) when rest != [] and project != "list" and project != "done" and project != "delete" do
    case split_at_in(rest) do
      {text_parts, [duration_str]} when text_parts != [] ->
        text = Enum.join(text_parts, " ")

        case parse_duration(duration_str) do
          {:ok, seconds} ->
            due_at = System.system_time(:second) + seconds

            case :alfred_scheduler.add_reminder(project, text, due_at) do
              {:ok, id} ->
                Butler.say(
                  "Rappel ##{id} noté, Monsieur. Je vous rappellerai \"#{text}\" pour le projet \"#{project}\" dans #{format_duration(seconds)}."
                )
            end

          :error ->
            Butler.say(
              "Monsieur, je ne comprends pas cette durée. Utilisez : 30m, 2h, 1d, 1w."
            )
        end

      _ ->
        Butler.say(
          "Monsieur, la syntaxe est : alfred remind <projet> <texte> in <durée>"
        )

        IO.puts("  Exemple : alfred remind \"Alfred Dev\" \"Finir les tests\" in 2h")
    end
  end

  def handle(["list"]) do
    case :alfred_scheduler.list_reminders() do
      {:ok, []} ->
        Butler.say("Aucun rappel programmé, Monsieur. Votre esprit est libre.")

      {:ok, reminders} ->
        pending = Enum.filter(reminders, &(&1.status == :pending))
        done = Enum.filter(reminders, &(&1.status == :done))

        Butler.say("Monsieur, voici vos rappels :\n")

        unless Enum.empty?(pending) do
          IO.puts("  En attente :")

          Enum.each(pending, fn r ->
            time_str = format_due_at(r.due_at)
            IO.puts("  ⏰ ##{r.id} [#{r.project}] #{r.text} — #{time_str}")
          end)
        end

        unless Enum.empty?(done) do
          IO.puts("\n  Accomplis :")

          Enum.each(done, fn r ->
            IO.puts("  ✓ ##{r.id} [#{r.project}] #{r.text}")
          end)
        end

        IO.puts("")
    end
  end

  def handle(["done", id_str]) do
    case Integer.parse(id_str) do
      {id, ""} ->
        case :alfred_scheduler.complete_reminder(id) do
          :ok ->
            Butler.say("Rappel ##{id} marqué comme accompli, Monsieur.")

          {:error, :not_found} ->
            Butler.say("Je suis navré Monsieur, le rappel ##{id} n'existe pas.")
        end

      _ ->
        Butler.say("Monsieur, veuillez indiquer un numéro de rappel valide.")
    end
  end

  def handle(["delete", id_str]) do
    case Integer.parse(id_str) do
      {id, ""} ->
        case :alfred_scheduler.delete_reminder(id) do
          :ok ->
            Butler.say("Rappel ##{id} supprimé, Monsieur.")

          {:error, :not_found} ->
            Butler.say("Je suis navré Monsieur, le rappel ##{id} n'existe pas.")
        end

      _ ->
        Butler.say("Monsieur, veuillez indiquer un numéro de rappel valide.")
    end
  end

  def handle(_) do
    Butler.say("Monsieur, les commandes de rappel sont :\n")

    IO.puts("""
      alfred remind <projet> <texte> in <durée>   Programmer un rappel
      alfred remind list                           Lister les rappels
      alfred remind done <id>                      Marquer comme accompli
      alfred remind delete <id>                    Supprimer un rappel

      Durées : 30m (minutes), 2h (heures), 1d (jours), 1w (semaines)
    """)
  end

  @doc """
  Vérifie les rappels en retard et les affiche.
  Appelé automatiquement lors du greeting.
  """
  def check_and_notify do
    case :alfred_scheduler.check_due() do
      {:ok, []} ->
        :ok

      {:ok, due_reminders} ->
        IO.puts("")
        IO.puts("  ⏰ Monsieur, vous avez #{length(due_reminders)} rappel(s) en retard :\n")

        Enum.each(due_reminders, fn r ->
          ago = format_ago(System.system_time(:second) - r.due_at)
          IO.puts("    → [#{r.project}] #{r.text}  (il y a #{ago})")
        end)

        IO.puts("")
        IO.puts("  Utilisez 'alfred remind done <id>' pour les marquer comme faits.")
        IO.puts("")
    end
  end

  # -- Parsing helpers --

  defp split_at_in(args) do
    case Enum.find_index(args, &(&1 == "in")) do
      nil -> {args, []}
      idx -> {Enum.take(args, idx), Enum.drop(args, idx + 1)}
    end
  end

  defp parse_duration(str) do
    cond do
      Regex.match?(~r/^\d+m$/, str) ->
        {mins, _} = Integer.parse(str)
        {:ok, mins * 60}

      Regex.match?(~r/^\d+h$/, str) ->
        {hours, _} = Integer.parse(str)
        {:ok, hours * 3600}

      Regex.match?(~r/^\d+d$/, str) ->
        {days, _} = Integer.parse(str)
        {:ok, days * 86400}

      Regex.match?(~r/^\d+w$/, str) ->
        {weeks, _} = Integer.parse(str)
        {:ok, weeks * 604_800}

      true ->
        :error
    end
  end

  defp format_duration(seconds) when seconds < 3600 do
    "#{div(seconds, 60)} minutes"
  end

  defp format_duration(seconds) when seconds < 86400 do
    "#{div(seconds, 3600)} heures"
  end

  defp format_duration(seconds) when seconds < 604_800 do
    "#{div(seconds, 86400)} jours"
  end

  defp format_duration(seconds) do
    "#{div(seconds, 604_800)} semaines"
  end

  defp format_due_at(due_at) do
    now = System.system_time(:second)

    if due_at <= now do
      "en retard de #{format_ago(now - due_at)}"
    else
      "dans #{format_ago(due_at - now)}"
    end
  end

  defp format_ago(seconds) when seconds < 60, do: "#{seconds}s"
  defp format_ago(seconds) when seconds < 3600, do: "#{div(seconds, 60)}min"
  defp format_ago(seconds) when seconds < 86400, do: "#{div(seconds, 3600)}h"
  defp format_ago(seconds), do: "#{div(seconds, 86400)}j"
end
