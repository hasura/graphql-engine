import type { HtmlAnalyticsAttributes } from '../types';

/**
 * Get the HTML attributes that will be override.
 */
export function getOverrideHtmlAttributes(
  currentHtmlAttributes: Record<string, string>,
  htmlAttributes: Record<string, string>,
): (keyof HtmlAnalyticsAttributes)[] {
  return Object.keys(htmlAttributes).filter(
    (htmlAttribute) =>
      currentHtmlAttributes[htmlAttribute] &&
      currentHtmlAttributes[htmlAttribute] !== htmlAttributes[htmlAttribute],
  ) as (keyof HtmlAnalyticsAttributes)[];
}
