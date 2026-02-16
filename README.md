# Alfred — Le Majordome Numérique

Alfred est un majordome numérique dévoué, fidèle et toujours disponible. Il aide son maître dans l'administration de ses projets, garde ses secrets et offre des éléments de réflexion grâce à son intelligence artificielle.

## Architecture

Alfred repose sur une architecture polyglotte à 6 langages, chacun incarnant un organe :

```
         Ada ←──┐
           R ←──┤
       Julia ←──┼─── Elixir (coeur) ←──→ Maître (CLI)
      Erlang ←──┤    (orchestre tout)
         Zig ←──┘
```

| Organe | Langage | Rôle | Statut |
|--------|---------|------|--------|
| Le Coeur | **Elixir** | CLI, orchestration, stockage | v0.1 |
| Les Os | **Zig** | Coffre-fort chiffré AES-256-GCM | v0.1 |
| Les Muscles | **Erlang** | Scheduler OTP, supervision, health | v0.1 |
| Le Cerveau | **Julia** | Analyse, résumés, suggestions | v0.1 |
| Le Cortex | **R** | Statistiques, tendances, graphiques | Prévu |
| Les Bras | **Ada** | Interaction système/matériel | Prévu |

Elixir est le hub central. Toutes les communications passent par lui. Les autres langages sont des organes qu'Elixir orchestre via des Erlang Ports (stdin/stdout JSON) ou la cohabitation native BEAM.

## Prérequis

- **Elixir** >= 1.15
- **Erlang/OTP** >= 26
- **Zig** >= 0.13 (pour le coffre-fort)
- **Julia** >= 1.10 (pour l'analyse)

## Installation

```bash
cd alfred

# Dépendances Elixir
mix deps.get

# Compiler le coffre-fort Zig
cd native/vault && zig build && cd ../..

# Compiler et créer l'exécutable
mix escript.build
```

## Commandes

### Projets & Tâches

```bash
alfred                                     # Salutation
alfred project new "Mon Projet"            # Créer un projet
alfred project list                        # Lister les projets
alfred project delete "Mon Projet"         # Supprimer un projet
alfred task add "Mon Projet" "Faire X"     # Ajouter une tâche
alfred task list [projet]                  # Lister les tâches
alfred task done <id>                      # Accomplir une tâche
alfred task priority <id> <1-5>            # Définir la priorité
alfred note add "Mon Projet" "Réflexion"   # Ajouter une note
alfred note list [projet]                  # Lister les notes
alfred status                              # Vue d'ensemble
```

### Coffre-fort (Zig)

```bash
alfred vault init                          # Créer le coffre (master password)
alfred vault store <clé> [valeur]          # Stocker un secret
alfred vault get <clé>                     # Récupérer un secret
alfred vault list                          # Lister les clés
alfred vault delete <clé>                  # Supprimer un secret
alfred vault note "Texte sensible"         # Note chiffrée
alfred vault notes                         # Lister les notes chiffrées
```

### Rappels (Erlang)

```bash
alfred remind "Projet" "Texte" in 2h       # Rappel dans 2 heures
alfred remind list                          # Lister les rappels
alfred remind done <id>                     # Accomplir un rappel
alfred remind delete <id>                   # Supprimer un rappel
```

Durées : `30m` (minutes), `2h` (heures), `1d` (jours), `1w` (semaines)

### Analyse (Julia)

```bash
alfred think about "Mon Projet"            # Analyse intelligente
alfred summarize "Mon Projet"              # Résumé du projet
alfred suggest                             # Suggestions transversales
```

### Diagnostic

```bash
alfred health                              # État de chaque organe
alfred help                                # Aide complète
```

## Tests

```bash
cd alfred && mix test
```

66 tests couvrant les 4 organes.

## Stockage

Les données sont stockées localement dans `~/.alfred/` :

- `data/projects.json` — Projets
- `data/tasks.json` — Tâches
- `data/notes.json` — Notes
- `data/reminders.dat` — Rappels (format Erlang binaire)
- `vault.enc` — Coffre-fort chiffré (AES-256-GCM)

## Licence

Projet personnel.
