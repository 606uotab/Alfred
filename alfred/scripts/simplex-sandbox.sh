#!/bin/bash
# Alfred — Lance simplex-chat dans un sandbox bubblewrap.
# Usage : bash alfred/scripts/simplex-sandbox.sh [port]
#
# - Filesystem en lecture seule sauf ~/.alfred/simplex/
# - PID namespace isolé
# - Meurt automatiquement si le parent meurt

set -e

PORT="${1:-5225}"
SIMPLEX_HOME="${HOME}/.alfred/simplex"
mkdir -p "${SIMPLEX_HOME}"

echo "  🔒 Lancement de simplex-chat en sandbox (port ${PORT})..."
echo "  📁 Données : ${SIMPLEX_HOME}"
echo ""

exec bwrap \
  --ro-bind /usr /usr \
  --ro-bind /lib /lib \
  --ro-bind /lib64 /lib64 \
  --ro-bind /bin /bin \
  --ro-bind /etc/resolv.conf /etc/resolv.conf \
  --ro-bind /etc/ssl /etc/ssl \
  --bind "${SIMPLEX_HOME}" "${HOME}/.simplex" \
  --proc /proc \
  --dev /dev \
  --tmpfs /tmp \
  --unshare-pid \
  --die-with-parent \
  simplex-chat -p "${PORT}"
