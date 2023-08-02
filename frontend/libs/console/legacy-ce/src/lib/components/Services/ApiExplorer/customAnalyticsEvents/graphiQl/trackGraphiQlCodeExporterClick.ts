import { trackCustomEvent } from '../../../../../features/Analytics';

/**
 * Track every interaction with the GraphiQL Code Exporter plugin.
 */
export function trackGraphiQlCodeExporterClick(
  button:
    | 'copy'
    | 'Fetch'
    | 'async/await'
    | 'async/await'
    | 'server-side usage'
    | 'JavaScript/TypeScript'
) {
  trackCustomEvent({
    location: 'GraphiQl > Code Exporter',
    action: 'click',
    object: button,
  });
}
