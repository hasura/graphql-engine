import { trackCustomEvent } from '@/features/Analytics';
import type { GraphiQlToolbarButtonClickEventNames } from './types';

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
  const eventName: GraphiQlToolbarButtonClickEventNames = `GraphiQl > Code Exporter > ${button} > click`;
  trackCustomEvent(eventName);
}
