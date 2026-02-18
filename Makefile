# Alfred — Makefile global
# Compile les 6 organes et construit l'escript CLI

.PHONY: all vault arms elixir clean test help

all: vault arms elixir
	@echo ""
	@echo "  ✓ Alfred est prêt. Lancez : ./alfred/alfred"
	@echo ""

# Os (Zig) — Coffre-fort chiffré
vault:
	@echo "  ── Compilation des Os (Zig)..."
	@cd alfred/native/vault && zig build 2>&1
	@echo "  ✓ Vault compilé"

# Bras (Ada) — Observation système
arms:
	@echo "  ── Compilation des Bras (Ada)..."
	@cd alfred/native/arms && make 2>&1
	@echo "  ✓ Arms compilé"

# Coeur (Elixir) — CLI escript
elixir:
	@echo "  ── Compilation du Coeur (Elixir)..."
	@cd alfred && mix deps.get --quiet 2>&1
	@cd alfred && mix escript.build 2>&1
	@echo "  ✓ Escript compilé"

# Tests
test:
	@cd alfred && mix test

# Nettoyage
clean:
	@cd alfred/native/vault && rm -rf zig-out zig-cache
	@cd alfred/native/arms && make clean
	@cd alfred && mix clean
	@echo "  ✓ Nettoyage terminé"

# Aide
help:
	@echo ""
	@echo "  Alfred — Makefile"
	@echo ""
	@echo "  make          Compiler tout (Zig + Ada + Elixir)"
	@echo "  make vault    Compiler le coffre-fort (Zig)"
	@echo "  make arms     Compiler les bras (Ada)"
	@echo "  make elixir   Compiler le coeur (Elixir escript)"
	@echo "  make test     Lancer les tests"
	@echo "  make clean    Nettoyer les artefacts"
	@echo ""
	@echo "  Prérequis : zig, gnat, elixir, julia, R"
	@echo ""
