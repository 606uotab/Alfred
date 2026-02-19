# Alfred --- Majordome Numerique

Un etre numerique polyglotte avec une architecture organique en 6 langages.
Il converse, execute des commandes, apprend de chaque interaction, et evolue au fil du temps.

```
$ alfred

  🎩 Alfred : Bonjour Monsieur. Comment puis-je vous etre utile ce matin ?

  ── Aperçu rapide ──
  3 projet(s), 5 tâche(s) en attente, 2 rappel(s)
  📊 50% d'accomplissement global
```

## Anatomie

```
       Mistral AI (langage)    Matrix/Element
            │                       │
         Ada ←──┐                   │
           R ←──┤                   │
       Julia ←──┼─── Elixir (coeur) ←──→ Maître (CLI / Shell / Element)
      Erlang ←──┤    (orchestre tout)
         Zig ←──┘
```

| Organe | Langage | Rôle | Lignes |
|--------|---------|------|--------|
| **Coeur** | Elixir | Hub central, CLI, chat, memoire, orchestration | 11 519 |
| **Cerveau** | Julia | Analyse IA, suggestions, priorisation | 1 411 |
| **Os** | Zig | Coffre-fort chiffre AES-256-GCM (3 vaults) | 836 |
| **Bras** | Ada | Observation systeme, backup, alertes | 711 |
| **Cortex** | R | Statistiques, tendances, correlations | 543 |
| **Muscles** | Erlang | Scheduler OTP, supervision, rappels | 374 |
| **Langage** | Mistral AI | Conversation intelligente avec function calling | --- |

Tous les organes communiquent via le protocole JSON stdin/stdout (Erlang Ports).

## Installation

### Prerequis

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

### Compilation et installation

```bash
git clone https://github.com/606uotab/Alfred.git
cd Alfred
make install    # Compile tout + symlink + completion bash
```

Ou manuellement :
```bash
make            # Compile Zig + Ada + Elixir
```

## Utilisation

### Gestion de projets

```bash
alfred project new MonProjet
alfred task add MonProjet "Implementer la feature X"
alfred task list
alfred task done 1
alfred note add MonProjet "Idee importante"
alfred note list
```

### Conversation (Mistral AI + function calling)

```bash
alfred chat                           # Mode conversation interactif
alfred ask "Quelle est la capitale du Japon ?"
alfred shell                          # Mode hybride : commandes + conversation
```

En mode chat, Alfred comprend le langage naturel et execute des actions :
- "Garde-moi une note sur Fanette : arroser les plantes" → execute `note add`
- "Cree un projet Vacances" → execute `project create`
- "Quelles sont mes taches ?" → execute `task list` et formule une reponse

### Rappels (Erlang)

```bash
alfred remind MonProjet "Deadline" in 2h
alfred remind list
alfred remind done 1
```

### Coffre-fort chiffre (Zig)

```bash
alfred vault setup                    # Creer les 3 coffres
alfred vault store creator ma_cle     # Stocker un secret
alfred vault get creator ma_cle       # Recuperer
alfred vault list creator             # Lister les cles
```

### Culture (base de connaissances)

```bash
alfred culture learn botanique "Les orchidees aiment l'humidite"
alfred culture search orchidees
alfred culture list
alfred culture suggestions            # Suggestions auto-extraites des conversations
```

### Intelligence artificielle (Julia)

```bash
alfred briefing                       # Synthese quotidienne
alfred think about MonProjet          # Analyse profonde
alfred summarize MonProjet            # Resume
alfred suggest                        # Suggestions transversales
alfred search "mot cle"               # Recherche universelle
alfred prioritize MonProjet           # Priorisation intelligente
```

### Statistiques (R)

```bash
alfred cortex trends                  # Tendances interactions
alfred cortex productivity            # Stats productivite
alfred cortex culture                 # Tendances culturelles
alfred cortex correlations            # Analyse croisee
```

### Systeme (Ada)

```bash
alfred arms status                    # Info machine
alfred arms disk                      # Espace disque
alfred arms memory                    # RAM / swap
alfred arms backup                    # Sauvegarde
```

### Soul --- Personnalite vivante

```bash
alfred soul                           # Voir les traits actuels
alfred soul init                      # Inscrire l'ame (coffre creator)
alfred soul history                   # Historique de l'evolution
alfred soul reset                     # Reinitialiser les traits
```

Les traits d'Alfred (formalite, humour, verbosite, curiosite, empathie, proactivite) evoluent automatiquement au fil des conversations.

### Daemon --- Mode veille

```bash
alfred daemon start                   # Demarre le daemon (foreground)
alfred daemon status                  # Etat du daemon
alfred daemon stop                    # Arreter
```

Le daemon verifie les rappels toutes les 60 secondes, execute la maintenance horaire, et consolide la memoire quotidiennement.

### Matrix/Element --- Bridge

```bash
alfred matrix connect                 # Connecter a une room Element
alfred matrix status                  # Etat du bridge
alfred matrix send "Hello"            # Envoyer un message
alfred matrix disconnect              # Deconnecter
```

Alfred ecoute une room Matrix et repond comme en mode chat, avec le meme function calling et la meme personnalite.

### Tableaux de bord

```bash
alfred dashboard                      # Vue unifiee complete
alfred status                         # Apercu rapide
alfred health                         # Diagnostic des organes
alfred help                           # Liste de toutes les commandes
```

## Architecture

```
Alfred/
├── Makefile                          # Build global (make / make install)
├── install.sh                        # Script d'installation
├── GENESE.md                         # Journal de creation
├── alfred/
│   ├── mix.exs                       # Projet Elixir
│   ├── alfred_completion.bash        # Completion bash
│   ├── lib/alfred/
│   │   ├── cli.ex                    # Point d'entree CLI (70+ commandes)
│   │   ├── butler.ex                 # Personnalite majordome
│   │   ├── colors.ex                 # Couleurs ANSI
│   │   ├── daemon.ex                 # Mode daemon (GenServer)
│   │   ├── application.ex            # Supervision OTP
│   │   ├── projects/                 # Projets, taches, notes
│   │   ├── vault/                    # Coffre-fort (Zig port)
│   │   ├── brain/                    # Cerveau (Julia port)
│   │   ├── cortex/                   # Cortex (R port)
│   │   ├── arms/                     # Bras (Ada port)
│   │   ├── memory/
│   │   │   ├── episodic.ex           # Conversations
│   │   │   ├── semantic.ex           # Faits + consolidation
│   │   │   ├── procedural.ex         # Patterns
│   │   │   └── learner.ex            # Pipeline d'apprentissage
│   │   ├── chat/
│   │   │   ├── client.ex             # Client Mistral AI
│   │   │   ├── commands.ex           # Chat + function calling
│   │   │   ├── tools.ex              # Outils pour Mistral
│   │   │   ├── session.ex            # Gestion de session
│   │   │   └── system_prompt.ex      # Identite d'Alfred
│   │   ├── matrix/
│   │   │   ├── client.ex             # Client HTTP Matrix
│   │   │   ├── bridge.ex             # Bridge GenServer
│   │   │   └── commands.ex           # CLI Matrix
│   │   ├── soul/
│   │   │   ├── state.ex              # Traits de personnalite
│   │   │   ├── evolver.ex            # Evolution Mistral-driven
│   │   │   └── commands.ex           # CLI soul
│   │   ├── daemon/
│   │   │   └── commands.ex           # CLI daemon
│   │   ├── culture/                  # Base de connaissances
│   │   └── remind/                   # Rappels
│   ├── src/
│   │   ├── alfred_scheduler.erl      # Scheduler gen_server
│   │   └── alfred_health.erl         # Health check (8 organes)
│   ├── native/
│   │   ├── vault/src/main.zig        # AES-256-GCM
│   │   ├── brain/src/main.jl         # Analyse Julia
│   │   ├── cortex/src/main.R         # Statistiques R
│   │   └── arms/src/alfred_arms.adb  # Systeme Ada
│   └── test/                         # 217 tests
└── ~/.alfred/                        # Donnees utilisateur
    ├── data/                         # Projets, taches, memoire, soul
    ├── vaults/                       # Coffres chiffres (3)
    └── backups/                      # Sauvegardes Ada
```

## Memoire

Alfred a une memoire persistante a 3 couches :

- **Episodique** : chaque conversation est enregistree et resumee
- **Semantique** : les faits importants sont extraits et consolides automatiquement
- **Procedurale** : les patterns comportementaux sont detectes au fil du temps

Apres chaque conversation, le pipeline d'apprentissage :
1. Sauvegarde l'episode
2. Extrait les faits (Mistral + Julia)
3. Resume la conversation (Julia)
4. Detecte les patterns (Julia)
5. Extrait des suggestions de culture (Julia)
6. Consolide les statistiques (R)
7. Fait evoluer la personnalite (Mistral, periodiquement)

## Securite

- Chiffrement AES-256-GCM pour tous les secrets
- 3 coffres separes : `creator`, `users`, `culture`
- Controle d'acces par role (maitre, admin, utilisateur)
- Derivation de cle SHA-256 (100 000 iterations)
- 100% local --- aucune donnee ne quitte la machine (sauf Mistral API et Matrix si active)

## Tests

```bash
make test    # 217 tests
```

## Licence

Projet personnel de Baptiste --- vibe-dev assiste par Claude.
