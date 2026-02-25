defmodule Alfred.Dashboard.Html do
  @moduledoc """
  Template HTML du dashboard d'Alfred — page unique avec auto-refresh.
  """

  def page do
    """
    <!DOCTYPE html>
    <html lang="fr">
    <head>
      <meta charset="utf-8">
      <meta name="viewport" content="width=device-width, initial-scale=1">
      <title>Alfred — Dashboard</title>
      <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body {
          font-family: 'Courier New', monospace;
          background: #0a0a0a;
          color: #e0e0e0;
          padding: 20px;
          max-width: 1200px;
          margin: 0 auto;
        }
        h1 {
          text-align: center;
          color: #d4a574;
          font-size: 1.8em;
          margin-bottom: 8px;
        }
        .subtitle {
          text-align: center;
          color: #666;
          font-size: 0.85em;
          margin-bottom: 24px;
        }
        .grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(340px, 1fr));
          gap: 16px;
        }
        .card {
          background: #1a1a1a;
          border: 1px solid #333;
          border-radius: 8px;
          padding: 16px;
        }
        .card h2 {
          color: #d4a574;
          font-size: 1em;
          margin-bottom: 12px;
          border-bottom: 1px solid #333;
          padding-bottom: 6px;
        }
        .row {
          display: flex;
          justify-content: space-between;
          padding: 3px 0;
          font-size: 0.85em;
        }
        .label { color: #888; }
        .value { color: #e0e0e0; }
        .ok { color: #4a9; }
        .warn { color: #d94; }
        .off { color: #666; }
        .journal-text {
          font-size: 0.82em;
          color: #bbb;
          line-height: 1.5;
          margin-top: 8px;
          font-style: italic;
        }
        .highlight {
          font-size: 0.8em;
          color: #888;
          margin-top: 4px;
        }
        .trait-bar {
          display: flex;
          align-items: center;
          margin: 2px 0;
          font-size: 0.82em;
        }
        .trait-bar .name { width: 100px; color: #888; }
        .trait-bar .bar {
          flex: 1;
          height: 8px;
          background: #333;
          border-radius: 4px;
          overflow: hidden;
        }
        .trait-bar .fill {
          height: 100%;
          background: #d4a574;
          border-radius: 4px;
        }
        .trait-bar .pct { width: 40px; text-align: right; color: #888; font-size: 0.8em; }
        #updated { text-align: center; color: #444; font-size: 0.75em; margin-top: 16px; }
      </style>
    </head>
    <body>
      <h1>Alfred</h1>
      <p class="subtitle">Majordome Numerique — Dashboard</p>

      <div class="grid">
        <div class="card" id="card-status">
          <h2>Etat</h2>
          <div id="status-content">Chargement...</div>
        </div>

        <div class="card" id="card-soul">
          <h2>Ame</h2>
          <div id="soul-content">Chargement...</div>
        </div>

        <div class="card" id="card-memory">
          <h2>Memoire</h2>
          <div id="memory-content">Chargement...</div>
        </div>

        <div class="card" id="card-library">
          <h2>Lecture</h2>
          <div id="library-content">Chargement...</div>
        </div>

        <div class="card" id="card-journal" style="grid-column: 1 / -1;">
          <h2>Journal</h2>
          <div id="journal-content">Chargement...</div>
        </div>
      </div>

      <p id="updated"></p>

      <script>
        function row(label, value, cls) {
          return `<div class="row"><span class="label">${label}</span><span class="value ${cls||''}">${value}</span></div>`;
        }

        function traitBar(name, value) {
          const pct = Math.round(value * 100);
          return `<div class="trait-bar">
            <span class="name">${name}</span>
            <div class="bar"><div class="fill" style="width:${pct}%"></div></div>
            <span class="pct">${pct}%</span>
          </div>`;
        }

        async function fetchData() {
          try {
            const [status, soul, memory, library, journal] = await Promise.all([
              fetch('/api/status').then(r => r.json()),
              fetch('/api/soul').then(r => r.json()),
              fetch('/api/memory').then(r => r.json()),
              fetch('/api/library').then(r => r.json()),
              fetch('/api/journal').then(r => r.json())
            ]);

            // Status
            let s = '';
            if (status.daemon && status.daemon.running) {
              s += row('Daemon', status.daemon.uptime, 'ok');
              s += row('Checks', status.daemon.checks);
            } else {
              s += row('Daemon', 'inactif', 'off');
            }
            if (status.bridge && status.bridge.running) {
              s += row('Bridge', status.bridge.connected ? 'connecte' : 'deconnecte', status.bridge.connected ? 'ok' : 'warn');
              s += row('Messages', status.bridge.messages);
            } else {
              s += row('Bridge', 'inactif', 'off');
            }
            document.getElementById('status-content').innerHTML = s;

            // Soul
            let sl = '';
            if (soul.soul) {
              sl += row('Humeur', soul.soul.mood || '?');
              if (soul.soul.traits) {
                for (const [k, v] of Object.entries(soul.soul.traits)) {
                  sl += traitBar(k, v);
                }
              }
            }
            if (soul.convictions) {
              sl += row('Convictions', `${soul.convictions.mature} mures / ${soul.convictions.total}`);
            }
            document.getElementById('soul-content').innerHTML = sl || 'Aucune donnee';

            // Memory
            let m = '';
            m += row('Episodes', memory.episodes || 0);
            m += row('Faits', memory.facts || 0);
            m += row('Patterns', memory.patterns || 0);
            m += row('Synthese', memory.synthesis ? 'oui' : 'non');
            m += row('Consolidation', memory.last_consolidation || 'jamais');
            document.getElementById('memory-content').innerHTML = m;

            // Library
            let l = '';
            if (library.current) {
              l += row('Livre', `"${library.current.title}"`);
              l += row('Auteur', library.current.author);
              l += row('Jour', `${library.current.day}/7`);
            } else {
              l += row('Lecture', 'aucune', 'off');
            }
            l += row('Livres lus', library.books_read || 0);
            if (library.history && library.history.length > 0) {
              l += '<div style="margin-top:8px;font-size:0.8em;color:#666">';
              library.history.forEach(h => { l += `- "${h.title}" de ${h.author}<br>`; });
              l += '</div>';
            }
            document.getElementById('library-content').innerHTML = l;

            // Journal
            let j = '';
            if (journal.entry) {
              j += `<div class="row"><span class="label">${journal.date || '?'}</span><span class="value">${journal.mood || '?'}</span></div>`;
              j += `<div class="journal-text">${journal.entry}</div>`;
              if (journal.highlights && journal.highlights.length > 0) {
                j += '<div class="highlight">' + journal.highlights.map(h => `- ${h}`).join('<br>') + '</div>';
              }
            } else {
              j += '<span class="off">Pas encore ecrit aujourd\\'hui.</span>';
            }
            document.getElementById('journal-content').innerHTML = j;

            document.getElementById('updated').textContent = 'Mis a jour : ' + new Date().toLocaleTimeString('fr-FR');
          } catch (e) {
            console.error('Erreur fetch:', e);
          }
        }

        fetchData();
        setInterval(fetchData, 30000);
      </script>
    </body>
    </html>
    """
  end
end
