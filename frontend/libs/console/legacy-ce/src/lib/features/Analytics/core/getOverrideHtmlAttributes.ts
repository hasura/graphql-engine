import type { HtmlAnalyticsAttributes } from '../types';

/**
 * Get the HTML attributes that will be override.
 */
export function getOverrideHtmlAttributes(
  currentHtmlAttributes: Record<string, string>,
  htmlAttributes: Record<string, string>
): (keyof HtmlAnalyticsAttributes)[] {
  return Object.keys(htmlAttributes).filter(
    // @ts-expect-error Object.keys return string[] instead of `(keyof HtmlAnalyticsAttributes)[]`
    (htmlAttribute: keyof HtmlAnalyticsAttributes) =>
      currentHtmlAttributes[htmlAttribute] &&
      currentHtmlAttributes[htmlAttribute] !== htmlAttributes[htmlAttribute]
  );
}
