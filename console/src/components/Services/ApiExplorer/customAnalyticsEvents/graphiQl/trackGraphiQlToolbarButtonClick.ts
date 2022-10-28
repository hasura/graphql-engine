import { trackCustomEvent } from '@/features/Analytics';
import type { GraphiQlToolbarButtonClickEventNames } from './types';

/**
 * Track all the clicks on the buttons at the top of GraphiQL.
 */
export function trackGraphiQlToolbarButtonClick(
  button:
    | 'REST'
    | 'Cache'
    | 'History'
    | 'Analyze'
    | 'Prettify'
    | 'Explorer'
    | 'Derive action'
    | 'Code Exporter'
) {
  const eventName: GraphiQlToolbarButtonClickEventNames = `GraphiQl > ${button} > click`;
  trackCustomEvent(eventName);
}
