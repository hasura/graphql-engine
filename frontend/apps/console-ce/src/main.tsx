import { createRoot } from 'react-dom/client';
import { ConsoleCeApp } from '@hasura/console-legacy-ce';
import { initAppearance } from '@hasura/shared/ui';

// Apply the stored light/dark appearance as the bundle executes, before the
// first React render (CSP-safe: bundle code, not an inline <script>). This
// reduces — but cannot fully guarantee no — flash on a cold reload, since
// index.html's static markup can paint before the bundle loads.
initAppearance();

const container = document.getElementById('content');
const root = createRoot(container!);
root.render(<ConsoleCeApp />);
