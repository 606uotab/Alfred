# Alfred — Majordome Numérique

Un majordome numérique avec une architecture organique en 6 langages.

```
$ ./alfred/alfred

  🎩 Alfred : Bonjour Monsieur. Comment puis-je vous être utile ce matin ?

  ── Aperçu rapide ──
  3 projet(s), 5 tâche(s) en attente, 2 rappel(s)
  📊 50% d'accomplissement global
```

## Anatomie

```
       Mistral AI (langage)
            │
         Ada ←──┐
           R ←──┤
       Julia ←──┼─── Elixir (coeur) ←──→ Maître (CLI)
      Erlang ←──┤    (orchestre tout)
         Zig ←──┘
```

| Organe | Langage | Rôle |
|--------|---------|------|
| **Coeur** | Elixir | Hub central, CLI, mémoire, orchestration |
| **Os** | Zig | Coffre-fort chiffré AES-256-GCM (3 vaults) |
| **Muscles** | Erlang | Scheduler OTP, supervision, rappels |
| **Cerveau** | Julia | Analyse IA, suggestions, priorisation |
| **Cortex** | R | Statistiques, tendances, corrélations |
| **Bras** | Ada | Observation système, backup, alertes |
| **Langage** | Mistral AI | Conversation intelligente |

Tous les organes communiquent via le protocole JSON stdin/stdout (Erlang Ports).

## Installation

### Prérequis

```bash
# Elixir + Erlang
sudo apt install elixir erlang

# Zig (0.13+)
# https://ziglang.org/download/

# Julia
curl -fsSL https://install.julialang.org | sh

# R
sudo apt install r-base

# Ada (GNAT)
sudo apt install gnat
```

### Compilation

```bash
git clone https://github.com/606uotab/Alfred.git
cd Alfred
make
```

## Utilisation

```bash
# Raccourci recommandé
alias alfred='./alfred/alfred'

# Gestion de projets
alfred project new MonProjet
alfred task add MonProjet "Implémenter la feature X"
alfred task list
alfred task done 1
alfred task priority 2 3

# Notes
alfred note add MonProjet "Idée importante"
alfred note list

# Rappels (Erlang)
alfred remind MonProjet "Deadline" in 2h
alfred remind list

# Coffre-fort chiffré (Zig)
alfred vault setup                    # Créer les 3 coffres
alfred vault store creator ma_cle     # Stocker un secret
alfred vault get creator ma_cle       # Récupérer
alfred vault list creator             # Lister les clés

# Culture (base de connaissances)
alfred culture learn botanique "Les orchidées aiment l'humidité"
alfred culture search orchidées
alfred culture list

# Intelligence artificielle (Julia)
alfred briefing                       # Synthèse quotidienne
alfred think about MonProjet          # Analyse profonde
alfred summarize MonProjet            # Résumé
alfred suggest                        # Suggestions transversales
alfred search "mot clé"               # Recherche universelle
alfred prioritize MonProjet           # Priorisation intelligente

# Statistiques (R)
alfred cortex trends                  # Tendances interactions
alfred cortex productivity            # Stats productivité
alfred cortex culture                 # Tendances culturelles
alfred cortex correlations            # Analyse croisée

# Système (Ada)
alfred arms status                    # Info machine
alfred arms disk                      # Espace disque
alfred arms memory                    # RAM / swap
alfred arms backup                    # Sauvegarde

# Conversation (Mistral AI)
alfred chat                           # Mode interactif
alfred ask "Quelle est la capitale du Japon ?"

# Tableaux de bord
alfred dashboard                      # Vue unifiée complète
alfred status                         # Aperçu rapide
alfred health                         # Diagnostic des organes
```

## Architecture

```
Alfred/
├── Makefile                          # Build global (make)
├── GENESE.md                         # Journal de création
├── alfred/
│   ├── mix.exs                       # Projet Elixir
│   ├── lib/alfred/
│   │   ├── cli.ex                    # Point d'entrée CLI
│   │   ├── butler.ex                 # Personnalité majordome
│   │   ├── colors.ex                 # Couleurs ANSI
│   │   ├── project_data.ex           # Données projet partagées
│   │   ├── application.ex            # Supervision OTP
│   │   ├── storage/local.ex          # Persistance JSON
│   │   ├── projects/                 # Projets, tâches, notes
│   │   ├── vault/                    # Coffre-fort (Zig port)
│   │   ├── brain/                    # Cerveau (Julia port)
│   │   ├── cortex/                   # Cortex (R port)
│   │   ├── arms/                     # Bras (Ada port)
│   │   ├── memory/
│   │   │   ├── episodic.ex           # Conversations
│   │   │   ├── semantic.ex           # Faits
│   │   │   ├── procedural.ex         # Patterns
│   │   │   └── learner.ex            # Pipeline d'apprentissage
│   │   ├── chat/                     # Conversation Mistral AI
│   │   ├── culture/                  # Base de connaissances
│   │   └── soul/                     # Identité personnalisable
│   ├── src/
│   │   ├── alfred_scheduler.erl      # Scheduler gen_server
│   │   └── alfred_health.erl         # Health check (8 organes)
│   ├── native/
│   │   ├── vault/src/main.zig        # AES-256-GCM
│   │   ├── brain/src/main.jl         # Analyse Julia
│   │   ├── cortex/src/main.R         # Statistiques R
│   │   └── arms/src/alfred_arms.adb  # Système Ada
│   └── test/                         # 163 tests
└── ~/.alfred/                        # Données utilisateur
    ├── data/                         # Projets, tâches, mémoire
    ├── vaults/                       # Coffres chiffrés (3)
    └── backups/                      # Sauvegardes Ada
```

## Mémoire

Alfred a une mémoire persistante à 3 couches :

- **Épisodique** : chaque conversation est enregistrée et résumée
- **Sémantique** : les faits importants sont extraits automatiquement
- **Procédurale** : les patterns comportementaux sont détectés au fil du temps

Après chaque conversation, le pipeline d'apprentissage :
1. Sauvegarde l'épisode
2. Extrait les faits (Julia)
3. Résume la conversation (Julia)
4. Détecte les patterns (Julia)
5. Extrait des suggestions de culture (Julia)
6. Consolide les statistiques (R)

## Sécurité

- Chiffrement AES-256-GCM pour tous les secrets
- 3 coffres séparés : `creator`, `users`, `culture`
- Contrôle d'accès par rôle (maître, admin, utilisateur)
- Dérivation de clé SHA-256 (100 000 itérations)
- 100% local — aucune donnée ne quitte la machine

## Tests

```bash
make test    # 163 tests
```

## Licence

Projet personnel de Baptiste — vibe-dev assisté par Claude.
