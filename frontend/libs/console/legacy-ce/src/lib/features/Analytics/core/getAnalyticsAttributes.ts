import type { RedactOptions } from './heap/types';
import type { HtmlAnalyticsAttributes, HtmlNameAttributes } from '../types';
import { getRedactAttributes } from './heap/getRedactAttributes';

export const DATA_ANALYTICS_ATTRIBUTE = 'data-analytics-name';

export type AnalyticsOptions = RedactOptions & {
  /**
   * @deprecated It is meant for the old components that already had a `data-trackid` attribute
   * before the new Analytics component exists.
   */
  legacyTrackIdAttribute?: boolean;
};

/**
 * Get the HTML attributes to be used in the DOM to set the various analytics options.
 */
export function getAnalyticsAttributes(
  name: string,
  options?: AnalyticsOptions
): HtmlAnalyticsAttributes {
  let htmlAttributes: HtmlNameAttributes = { [DATA_ANALYTICS_ATTRIBUTE]: name };

  if (!options) return htmlAttributes;

  htmlAttributes = {
    ...htmlAttributes,

    ...getRedactAttributes({
      redactText: options.redactText,
      htmlAttributesToRedact: options.htmlAttributesToRedact,
    }),
  };

  if (options.legacyTrackIdAttribute) {
    htmlAttributes['data-trackid'] = name;
  }

  return htmlAttributes;
}
