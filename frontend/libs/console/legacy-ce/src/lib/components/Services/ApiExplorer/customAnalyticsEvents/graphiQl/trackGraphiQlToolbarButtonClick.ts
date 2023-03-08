import { trackCustomEvent } from '../../../../../features/Analytics';

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
  trackCustomEvent({
    location: 'GraphiQl',
    action: 'click',
    object: button,
  });
}
