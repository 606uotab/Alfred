#!/bin/bash
# Alfred — Lance simplex-chat dans un sandbox bubblewrap.
# Usage : bash alfred/scripts/simplex-sandbox.sh [port]
#
# - Filesystem en lecture seule sauf ~/.alfred/simplex/
# - PID namespace isolé
# - Meurt automatiquement si le parent meurt

set -e

PORT="${1:-5227}"
SIMPLEX_HOME="${HOME}/.alfred/simplex"
mkdir -p "${SIMPLEX_HOME}"

# Trouver le binaire simplex-chat
SIMPLEX_BIN="$(command -v simplex-chat 2>/dev/null || echo "${HOME}/.local/bin/simplex-chat")"
if [ ! -x "${SIMPLEX_BIN}" ]; then
    echo "  ✗ simplex-chat introuvable. Installez-le :"
    echo "    wget -qO- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash"
    exit 1
fi

# Tuer une instance précédente sur ce port si besoin
EXISTING=$(ss -tlnp 2>/dev/null | grep ":${PORT}" | grep -o 'pid=[0-9]*' | head -1 | cut -d= -f2)
if [ -n "${EXISTING}" ]; then
    echo "  ⚠ Port ${PORT} occupé (PID ${EXISTING}), arrêt..."
    kill "${EXISTING}" 2>/dev/null
    sleep 1
fi

echo "  🔒 Lancement de simplex-chat en sandbox (port ${PORT})..."
echo "  📁 Données : ${SIMPLEX_HOME}"
echo "  🔧 Binaire : ${SIMPLEX_BIN}"
echo ""

exec bwrap \
  --ro-bind /usr /usr \
  --ro-bind /lib /lib \
  --ro-bind /lib64 /lib64 \
  --ro-bind /bin /bin \
  --ro-bind /etc/resolv.conf /etc/resolv.conf \
  --ro-bind /etc/ssl /etc/ssl \
  --ro-bind "${SIMPLEX_BIN}" /opt/simplex-chat \
  --bind "${SIMPLEX_HOME}" "${HOME}/.simplex" \
  --proc /proc \
  --dev /dev \
  --tmpfs /tmp \
  --unshare-pid \
  --die-with-parent \
  /opt/simplex-chat -p "${PORT}"
