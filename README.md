# Alfred --- Majordome Numerique

Un etre numerique polyglotte avec une architecture organique en 6 langages.
Il converse, execute des commandes, apprend de chaque interaction, et evolue au fil du temps.

```
$ alfred daemon
  Mot de passe : ********
  🎩 Alfred : Bien, Monsieur. Demarrage d'Alfred...
  ✓ Alfred demarre (PID 42)
    Log  : ~/.alfred/alfred.log
    Stop : alfred daemon stop
```

## Anatomie

```
       Mistral AI (langage)    SimpleX Chat
            │                       │
         Ada ←──┐                   │
           R ←──┤                   │
       Julia ←──┼─── Elixir (coeur) ←──→ Maitre (CLI / Shell / SimpleX)
      Erlang ←──┤    (orchestre tout)
         Zig ←──┘
```

| Organe | Langage | Role | Lignes |
|--------|---------|------|--------|
| **Coeur** | Elixir | Hub central, CLI, chat, memoire, orchestration | ~14 100 |
| **Cerveau** | Julia | Analyse IA, suggestions, priorisation | ~1 400 |
| **Os** | Zig | Coffre-fort chiffre AES-256-GCM (3 vaults) | ~840 |
| **Bras** | Ada | Observation systeme, backup, alertes | ~710 |
| **Cortex** | R | Statistiques, tendances, correlations | ~540 |
| **Muscles** | Erlang | Scheduler OTP, supervision, rappels | ~370 |
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

# TTS (optionnel)
sudo apt install espeak-ng
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

## Demarrage rapide

```bash
# Premiere utilisation : configurer les coffres-forts
alfred vault setup

# Stocker la cle Mistral AI
alfred vault store creator mistral_api_key

# Demarrer Alfred (daemon complet)
alfred daemon
```

Alfred demande le mot de passe du coffre-fort, verifie l'identite, puis lance en arriere-plan :
- Le sandbox SimpleX Chat (bwrap)
- Le bridge de messagerie (WebSocket)
- Le daemon (taches planifiees, rappels, initiatives)

## Utilisation

### Gestion de projets

```bash
alfred project new MonProjet
alfred task add MonProjet "Implementer la feature X"
alfred task list
alfred task done 1
alfred note add MonProjet "Idee importante"
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
```

Les traits d'Alfred (formalite, humour, verbosite, curiosite, empathie, proactivite) evoluent automatiquement au fil des conversations.

### Daemon

```bash
alfred daemon                         # Demarrer Alfred (tout-en-un)
alfred daemon status                  # Etat du daemon
alfred daemon stop                    # Arreter Alfred
alfred daemon log                     # Voir les derniers logs
```

Le daemon orchestre :
- Rappels toutes les 60s
- Initiatives proactives toutes les 30 min (taches en retard, rappels imminents)
- Lecture quotidienne a 14h
- Rapport d'activite a 17h30
- Briefing news a 8h (via API locale)
- Journal intime a 22h (introspection Mistral)
- Consolidation memoire a 3h (archivage, decay, synthese)
- Notifications intelligentes (apprend les heures d'activite)

### SimpleX Chat --- Bridge

```bash
alfred simplex connect                # Connecter manuellement
alfred simplex status                 # Etat du bridge
alfred simplex send "Hello"           # Envoyer un message
```

En mode daemon, le bridge demarre automatiquement. Alfred ecoute via SimpleX Chat (WebSocket local, sandbox bwrap) et repond comme en mode chat.

Commandes SimpleX :
```
/status    /report    /health    /help
/library   /journal   /news      /soul
/memory    /voice     /dashboard /system
```

### Briefing matinal

```bash
alfred news                           # Dernier briefing
alfred news refresh                   # Generer un briefing frais
alfred news list                      # Briefings archives
```

Alfred lit les infos chaque matin a 8h depuis une API locale, les resume via Mistral par theme (monde, tech, finance, crypto), et envoie un teaser sur SimpleX.

### Journal intime

```bash
alfred journal                        # Derniere entree
alfred journal list                   # Entrees recentes
alfred journal write                  # Ecrire manuellement
alfred journal show 2026-02-26        # Entree d'une date
```

Chaque soir a 22h, Alfred ecrit dans son journal : reflexions sur la journee, humeur, points marquants, contexte (projets, lectures, conversations).

### Voix (TTS)

```bash
alfred voice                          # Statut
alfred voice on                       # Activer
alfred voice off                      # Desactiver
alfred voice say "Bonjour Monsieur"   # Tester
```

TTS via `espeak-ng`. Quand active, Alfred lit ses notifications et rappels a voix haute.

### Memoire consolidee

```bash
alfred memory facts                   # Faits memorises
alfred memory episodes                # Historique des conversations
alfred memory stats                   # Statistiques memoire
alfred memory consolidate             # Lancer la consolidation
```

Pipeline nocturne a 3h :
1. Archive les episodes > 7 jours
2. Oublie les faits a tres basse confiance (> 300 jours)
3. Elague les patterns obsoletes
4. Genere une synthese via Mistral (injectee dans le system prompt)

### Dashboard web

```bash
alfred dashboard web                  # Demarrer sur http://localhost:4567
```

Interface web dark theme, vanilla JS, auto-refresh 30s. Sections : etat, ame, memoire, lecture, journal.

### Tableaux de bord CLI

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
│   ├── lib/alfred/
│   │   ├── cli.ex                    # Point d'entree CLI (80+ commandes)
│   │   ├── butler.ex                 # Personnalite majordome
│   │   ├── launcher.ex              # Orchestrateur de demarrage
│   │   ├── daemon.ex                # Mode daemon (GenServer, 11 slots)
│   │   ├── log.ex                   # Logger fichier (zero stdout)
│   │   ├── application.ex           # Supervision OTP
│   │   ├── projects/                # Projets, taches, notes
│   │   ├── vault/                   # Coffre-fort (Zig port)
│   │   ├── brain/                   # Cerveau (Julia port)
│   │   ├── cortex/                  # Cortex (R port)
│   │   ├── arms/                    # Bras (Ada port)
│   │   ├── memory/
│   │   │   ├── episodic.ex          # Conversations
│   │   │   ├── semantic.ex          # Faits + consolidation
│   │   │   ├── procedural.ex        # Patterns
│   │   │   ├── learner.ex           # Pipeline d'apprentissage
│   │   │   └── consolidator.ex      # Pipeline nocturne
│   │   ├── chat/
│   │   │   ├── client.ex            # Client Mistral AI
│   │   │   ├── commands.ex          # Chat + function calling
│   │   │   ├── tools.ex             # Outils pour Mistral
│   │   │   ├── session.ex           # Gestion de session
│   │   │   └── system_prompt.ex     # Identite d'Alfred
│   │   ├── simplex/
│   │   │   ├── websocket.ex         # Client WebSocket RFC 6455
│   │   │   ├── client.ex            # API SimpleX Chat
│   │   │   └── bridge.ex            # Bridge GenServer + commandes
│   │   ├── soul/
│   │   │   ├── state.ex             # Traits de personnalite
│   │   │   ├── evolver.ex           # Evolution Mistral-driven
│   │   │   └── conviction_evolver.ex # Convictions forgees
│   │   ├── initiative/
│   │   │   └── smart.ex             # Notifications intelligentes
│   │   ├── dashboard/
│   │   │   ├── server.ex            # HTTP :gen_tcp port 4567
│   │   │   └── html.ex              # Template dark theme
│   │   ├── journal.ex               # Journal intime quotidien
│   │   ├── news.ex                  # Briefing matinal
│   │   ├── voice.ex                 # TTS espeak-ng
│   │   ├── library/                 # Lecteur hebdomadaire
│   │   ├── culture/                 # Base de connaissances
│   │   └── remind/                  # Rappels
│   ├── src/
│   │   ├── alfred_scheduler.erl     # Scheduler gen_server
│   │   └── alfred_health.erl        # Health check (8 organes)
│   ├── native/
│   │   ├── vault/src/main.zig       # AES-256-GCM
│   │   ├── brain/src/main.jl        # Analyse Julia
│   │   ├── cortex/src/main.R        # Statistiques R
│   │   └── arms/src/alfred_arms.adb # Systeme Ada
│   └── test/                        # 340 tests, 23 fichiers
└── ~/.alfred/                       # Donnees utilisateur
    ├── alfred.log                   # Log du daemon
    ├── alfred.pid                   # PID du daemon
    ├── data/                        # Projets, taches, memoire, soul
    │   ├── journal/                 # Entrees du journal (YYYY-MM-DD.json)
    │   ├── news/                    # Briefings matinaux
    │   ├── reports/                 # Rapports quotidiens
    │   ├── memory/                  # Synthese, logs consolidation
    │   └── initiative/              # Logs d'interactions
    ├── simplex/                     # Donnees SimpleX Chat (sandbox)
    ├── vaults/                      # Coffres chiffres (3)
    └── backups/                     # Sauvegardes Ada
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

Toutes les nuits a 3h, la consolidation :
1. Archive les vieux episodes (> 7 jours)
2. Oublie les faits a tres basse confiance (> 300 jours)
3. Elague les patterns obsoletes
4. Genere une synthese memoire (Mistral) injectee dans le system prompt

## Securite

- Chiffrement AES-256-GCM pour tous les secrets
- 3 coffres separes : `creator`, `users`, `culture`
- Controle d'acces par role (maitre, admin, utilisateur)
- Derivation de cle SHA-256 (100 000 iterations)
- SimpleX Chat en sandbox bwrap (isolation filesystem)
- 100% local --- aucune donnee ne quitte la machine (sauf Mistral API et SimpleX si actif)

## Tests

```bash
make test    # 340 tests, 0 failures
```

## Licence

Projet personnel de Baptiste --- vibe-dev assiste par Claude.
