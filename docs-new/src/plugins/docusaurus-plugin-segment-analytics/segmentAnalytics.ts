import ExecutionEnvironment from '@docusaurus/ExecutionEnvironment';
import globalData from '@generated/globalData';
import type { PluginOptions } from './plugin-segment-analytics';

export default (function segmentAnalyticsModule() {
  if (!ExecutionEnvironment.canUseDOM) return null;

  const {trackPage, trackPageDelay} = globalData['docusaurus-plugin-segment-analytics']
    .default as PluginOptions;

  return {
    onRouteUpdate({location}: {location: Location}) {
      // Always refer to the variable on window in case it gets overridden elsewhere.
      if (!trackPage) return;

      // Adding a delay (defaults to 50ms when not provided by plugin option `trackPageDelay`)
    // ensure that the segment route tracking is in sync with the actual Gatsby route
    // (otherwise you can end up in a state where the Segment page tracking reports
    // the previous page on route change).
      const delay = Math.max(0, trackPageDelay)

      window.setTimeout(() => {
        window.analytics && window.analytics.page(document.title);
      }, delay);
    },
  };
})();