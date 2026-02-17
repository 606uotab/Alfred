# Alfred — Architecture Technique

## Vue d'ensemble

Alfred est un être numérique polyglotte. Chaque langage incarne un organe avec une responsabilité précise. Elixir est le coeur qui orchestre tout.

## Organes

### Elixir — Le Coeur
- **Rôle** : Orchestration, CLI, stockage, conversation
- **Fichiers** : `lib/alfred/`
- **Pattern** : Escript CLI, modules fonctionnels, stockage JSON fichier
- **Communication** : Appelle les autres organes via Erlang Ports ou BEAM natif

### Zig — Les Os
- **Rôle** : Coffre-fort chiffré (AES-256-GCM, SHA-256 key derivation)
- **Fichiers** : `native/vault/src/`
- **Communication** : Erlang Port, JSON stdin/stdout
- **Sécurité** : Clé dérivée du master password, nonce aléatoire par écriture

### Erlang — Les Muscles
- **Rôle** : Scheduler OTP gen_server, supervision, health monitor
- **Fichiers** : `src/alfred_scheduler.erl`, `src/alfred_health.erl`
- **Communication** : BEAM natif (même VM qu'Elixir)
- **Persistance** : `term_to_binary` pour les rappels

### Julia — Le Cerveau
- **Rôle** : Analyse de projets, extraction de faits, détection de patterns
- **Fichiers** : `native/brain/src/main.jl`
- **Communication** : Erlang Port, JSON stdin/stdout
- **Commandes** : analyze, summarize, suggest, extract_facts, summarize_episode, detect_patterns

### R — Le Cortex
- **Rôle** : Statistiques à long terme, tendances, analyse comportementale
- **Fichiers** : `native/cortex/src/main.R`
- **Communication** : Erlang Port, JSON stdin/stdout (package jsonlite)
- **Commandes** : interaction_trends, memory_stats, behavioral_analysis

### Mistral AI — Le Langage
- **Rôle** : Compréhension et génération de langage naturel
- **Communication** : HTTP API via :httpc (Erlang built-in)
- **Auth** : Token stocké chiffré dans le Vault Zig

## Mémoire

```
Conversation → Mémoire épisodique (fichiers JSON par session)
                    ↓
              Extraction de faits (Mistral ou Julia)
                    ↓
              Mémoire sémantique (semantic.json — faits structurés)
                    ↓
              Détection de patterns (Julia, consolidation R)
                    ↓
              Mémoire procédurale (procedural.json — patterns)
```

### Flux de conversation
1. Token Mistral récupéré du Vault (une fois par session)
2. Mémoire pertinente chargée (faits + épisodes récents + patterns)
3. System prompt construit (personnalité + contexte mémorisé)
4. Échange avec Mistral API
5. Sauvegarde épisode + extraction de faits

## Protocole d'échange inter-organes

Tous les organes externes (Zig, Julia, R) suivent le même protocole :
- Elixir ouvre un Erlang Port (`Port.open/2`)
- Envoi : une ligne JSON sur stdin
- Réception : une ligne JSON sur stdout
- Format réponse : `{"status": "ok", ...}` ou `{"status": "error", "message": "..."}`
- Retour Elixir : `{:ok, map}` ou `{:error, string}`

## Dépendances

- **Elixir** : Jason (JSON) — seule dépendance externe
- **Erlang** : :inets, :ssl (HTTP built-in) — zéro dépendance externe
- **Julia** : JSON3, Statistics, Dates
- **R** : jsonlite
- **Zig** : zéro dépendance (stdlib uniquement)
