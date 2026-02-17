# Alfred — Le Majordome Numérique

Alfred est un être numérique dévoué, fidèle et toujours disponible. Il aide son maître dans l'administration de ses projets, garde ses secrets, converse avec intelligence et se souvient de chaque échange.

## Architecture

Architecture polyglotte à 6 langages, chacun incarnant un organe. Elixir orchestre tout — les autres communiquent via Erlang Ports (JSON stdin/stdout) ou cohabitation BEAM native.

```
       Mistral AI (langage)
            │
         Ada ←──┐
           R ←──┤
       Julia ←──┼─── Elixir (coeur) ←──→ Maître (CLI)
      Erlang ←──┤    (orchestre tout)
         Zig ←──┘
```

| Organe | Langage | Rôle | Statut |
|--------|---------|------|--------|
| Le Coeur | **Elixir** | CLI, orchestration, stockage, conversation | v0.2 |
| Les Os | **Zig** | Coffre-fort chiffré AES-256-GCM | v0.1 |
| Les Muscles | **Erlang** | Scheduler OTP, supervision, health | v0.1 |
| Le Cerveau | **Julia** | Analyse, résumés, extraction de faits, patterns | v0.2 |
| Le Cortex | **R** | Statistiques, tendances, analyse comportementale | v0.2 |
| Le Langage | **Mistral AI** | Conversation intelligente via API | v0.2 |
| Les Bras | **Ada** | Interaction système/matériel | Prévu |

## Mémoire

Alfred possède une mémoire persistante à 3 couches :

- **Épisodique** — Chaque conversation est sauvée et résumée
- **Sémantique** — Faits extraits sur le maître (préférences, connaissances, contexte)
- **Procédurale** — Patterns comportementaux détectés au fil du temps

Après chaque conversation, Alfred extrait les faits importants et les mémorise. Avant chaque réponse, il charge les souvenirs pertinents dans son contexte.

## Prérequis

- **Elixir** >= 1.15 / **Erlang/OTP** >= 26
- **Zig** >= 0.13 (coffre-fort)
- **Julia** >= 1.10 (analyse)
- **R** >= 4.0 + jsonlite (statistiques)
- **Clé API Mistral** (conversation)

## Installation

```bash
cd alfred
mix deps.get

# Coffre-fort Zig
cd native/vault && zig build && cd ../..

# Build
mix escript.build

# Stocker la clé Mistral dans le coffre-fort
./alfred vault init
./alfred vault store mistral_api_key
```

## Commandes

### Conversation (Mistral AI)

```bash
alfred chat                                # Conversation interactive
alfred ask "Question ?"                    # Question ponctuelle
```

### Mémoire

```bash
alfred memory facts                        # Faits mémorisés
alfred memory search <mots>                # Rechercher dans la mémoire
alfred memory episodes                     # Historique des conversations
alfred memory patterns                     # Patterns détectés
alfred memory forget <id>                  # Oublier un fait
```

### Projets & Tâches

```bash
alfred project new/list/delete <nom>       # Gestion de projets
alfred task add <projet> <desc>            # Ajouter une tâche
alfred task list [projet]                  # Lister les tâches
alfred task done <id>                      # Accomplir une tâche
alfred task priority <id> <1-5>            # Priorité
alfred note add <projet> <texte>           # Notes
alfred status                              # Vue d'ensemble
```

### Coffre-fort (Zig)

```bash
alfred vault init                          # Créer le coffre
alfred vault store/get/list/delete <clé>   # Gérer les secrets
alfred vault note/notes                    # Notes chiffrées
```

### Rappels (Erlang)

```bash
alfred remind <projet> <texte> in <durée>  # 30m, 2h, 1d, 1w
alfred remind list/done/delete <id>        # Gérer les rappels
```

### Analyse (Julia)

```bash
alfred think about <projet>                # Analyse intelligente
alfred summarize <projet>                  # Résumé du projet
alfred suggest                             # Suggestions transversales
```

### Cortex (R)

```bash
alfred cortex trends                       # Tendances des interactions
alfred cortex stats                        # Statistiques de mémoire
alfred cortex analyze                      # Analyse comportementale
```

### Diagnostic

```bash
alfred health                              # État de chaque organe (7 checks)
alfred help                                # Aide complète
```

## Tests

```bash
cd alfred && mix test    # 96 tests, 6 organes
```

## Stockage

```
~/.alfred/
  data/
    projects.json                          # Projets
    tasks.json                             # Tâches
    notes.json                             # Notes
    reminders.dat                          # Rappels (Erlang binary)
    memory/
      semantic.json                        # Faits mémorisés
      procedural.json                      # Patterns comportementaux
      episodes/                            # Conversations (1 fichier/session)
  vault.enc                                # Coffre-fort chiffré (AES-256-GCM)
```

## Licence

Projet personnel et expérimental.
