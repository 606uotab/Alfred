#!/bin/bash
# Alfred — Script d'installation
# Compile, installe le symlink et la completion bash.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ALFRED_DIR="${SCRIPT_DIR}/alfred"
ALFRED_HOME="${HOME}/.alfred"

echo ""
echo "  🎩 Installation d'Alfred — Majordome Numérique"
echo ""

# 1. Compile l'escript Elixir
echo "  ── Compilation du coeur (Elixir)..."
cd "${ALFRED_DIR}"
mix deps.get --quiet 2>/dev/null || true
mix escript.build 2>/dev/null
echo "  ✓ Escript compilé"

# 2. Symlink dans /usr/local/bin (ou ~/bin si pas root)
INSTALL_DIR="/usr/local/bin"
if [ ! -w "${INSTALL_DIR}" ]; then
    INSTALL_DIR="${HOME}/bin"
    mkdir -p "${INSTALL_DIR}"
fi

ln -sf "${ALFRED_DIR}/alfred" "${INSTALL_DIR}/alfred"
echo "  ✓ Symlink créé : ${INSTALL_DIR}/alfred"

# 3. Installe la completion bash
mkdir -p "${ALFRED_HOME}"
cp "${ALFRED_DIR}/alfred_completion.bash" "${ALFRED_HOME}/alfred_completion.bash"
echo "  ✓ Completion bash installée"

# 4. Ajoute ~/bin au PATH si nécessaire
BASHRC="${HOME}/.bashrc"

if [ "${INSTALL_DIR}" = "${HOME}/bin" ]; then
    PATH_LINE='export PATH="${HOME}/bin:${PATH}"'
    if [ -f "${BASHRC}" ] && ! grep -q '${HOME}/bin' "${BASHRC}" && ! grep -q '$HOME/bin' "${BASHRC}"; then
        echo "" >> "${BASHRC}"
        echo "# Alfred PATH" >> "${BASHRC}"
        echo "${PATH_LINE}" >> "${BASHRC}"
        echo "  ✓ ~/bin ajouté au PATH dans ~/.bashrc"
    fi
fi

# 5. Ajoute la completion dans ~/.bashrc si absent
SOURCE_LINE="[ -f ~/.alfred/alfred_completion.bash ] && source ~/.alfred/alfred_completion.bash"

if [ -f "${BASHRC}" ]; then
    if ! grep -q "alfred_completion.bash" "${BASHRC}"; then
        echo "" >> "${BASHRC}"
        echo "# Alfred completion" >> "${BASHRC}"
        echo "${SOURCE_LINE}" >> "${BASHRC}"
        echo "  ✓ Completion ajoutée à ~/.bashrc"
    else
        echo "  ✓ Completion déjà dans ~/.bashrc"
    fi
else
    echo "${PATH_LINE}" > "${BASHRC}"
    echo "# Alfred completion" >> "${BASHRC}"
    echo "${SOURCE_LINE}" >> "${BASHRC}"
    echo "  ✓ ~/.bashrc créé avec completion"
fi

echo ""
echo "  🎩 Alfred est installé, Monsieur."
echo ""
echo "  Commandes :"
echo "    alfred              Salutation"
echo "    alfred help          Aide complète"
echo "    alfred shell         Mode interactif"
echo "    alfred chat          Conversation IA"
echo ""
echo "  Rechargez votre terminal : source ~/.bashrc"
echo ""
