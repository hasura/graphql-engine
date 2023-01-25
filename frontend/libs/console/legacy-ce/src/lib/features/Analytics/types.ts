import type { HtmlRedactAttributes } from './core/heap/types';

export type HtmlNameAttributes = {
  /**
   * This attribute
   * - helps the developer to identify the passed `component` right from the DOM without any debugging
   * - helps identifying the component in the various analytics tools
   *
   * ATTENTION: remember to update the ESLint react/forbid-dom-props rule too if you edit the attribute.
   */
  'data-analytics-name': string;

  /**
   * At the beginning, the `data-analytics-name` was `data-trackid` and some components already use it.
   * Now, `data-trackid` is considered legacy.
   *
   * ATTENTION: remember to update the ESLint react/forbid-dom-props rule too if you edit the attribute.
   */
  'data-trackid'?: string;
};

export type HtmlAnalyticsAttributes = HtmlRedactAttributes & HtmlNameAttributes;
