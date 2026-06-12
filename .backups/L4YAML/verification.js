
// Search functionality
const searchInput = document.getElementById('search-input');
const searchResults = document.getElementById('search-results');

let declarations = [];

// Load declaration index
async function loadDeclarations() {
  try {
    const response = await fetch('declarations.json');
    declarations = await response.json();
  } catch (e) {
    console.error('Failed to load declarations:', e);
  }
}

// Search function
function search(query) {
  if (!query || query.length < 2) {
    searchResults.innerHTML = '';
    return;
  }

  const q = query.toLowerCase();
  const matches = declarations.filter(d =>
    d.name.toLowerCase().includes(q) ||
    (d.module && d.module.toLowerCase().includes(q))
  ).slice(0, 50);

  if (matches.length === 0) {
    searchResults.innerHTML = '<p>No results found.</p>';
    return;
  }

  const html = matches.map(d => `
    <div class="decl-card">
      <div class="decl-header">
        <span class="decl-name">${escapeHtml(d.name)}</span>
        <span class="badge badge-${d.status}">${d.status}</span>
      </div>
      <div class="decl-body">
        <a href="modules/${d.module.replace(/\./g, '_')}.html#${d.name}">View in ${d.module}</a>
      </div>
    </div>
  `).join('');

  searchResults.innerHTML = html;
}

function escapeHtml(s) {
  return s.replace(/&/g, '&amp;')
          .replace(/</g, '&lt;')
          .replace(/>/g, '&gt;')
          .replace(/"/g, '&quot;');
}

// Event listeners
if (searchInput) {
  searchInput.addEventListener('input', (e) => search(e.target.value));
  loadDeclarations();
}

// Collapsible navigation
document.querySelectorAll('.nav-toggle').forEach(toggle => {
  toggle.addEventListener('click', () => {
    toggle.parentElement.classList.toggle('expanded');
  });
});
