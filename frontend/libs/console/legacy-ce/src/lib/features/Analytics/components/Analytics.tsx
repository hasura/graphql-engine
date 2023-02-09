import type { ReactNode, ReactPortal } from 'react';
import * as React from 'react';

import type { AnalyticsOptions } from '../core/getAnalyticsAttributes';
import { useGetAnalyticsAttributes } from '../hooks/useGetAnalyticsAttributes';
import { getOverrideHtmlAttributes } from '../core/getOverrideHtmlAttributes';
import { programmaticallyTraceError } from '../core/programmaticallyTraceError';

import { detectChildrenType } from './core/detectChildrenType';
import { warnIfTitleElementIsPassed } from './core/warnIfTitleElementIsPassed';
import { getHtmlAttributesInjectionMode } from './core/getHtmlAttributesInjectionMode';
import { warnIfPassingHtmlAttributesToTexts } from './core/warnIfPassingHtmlAttributesToTexts';
import { childrenAreKnownToAcceptHTMLAttributes } from './core/childrenAreKnownToAcceptHTMLAttributes';
import { warnIfPassingHtmlAttributesToHtmlElements } from './core/warnIfPassingHtmlAttributesToHtmlElements';
import { warnIfWrappingChildrenAcceptingHtmlAttributes } from './core/warnIfWrappingChildrenAcceptingHtmlAttributes';

type AnalyticsProps = {
  /** It's called `name` instead of id because it does not have to be unique 😊 */
  name: string;

  /* React portal would require a custom management of the container to properly add the HTML attributes. */
  children: Exclude<ReactNode, ReactPortal>;

  /* Force to pass the HTML attributes to the children, even if they are React components instead of DOM elements */
  passHtmlAttributesToChildren?: boolean;
} & AnalyticsOptions;

/**
 * Useful to add all the HTML stuff needed to everything tracking. Add
 * - some HTML attributes to the passed HTML elements
 * - or add a <div style="display:contents"> around React components
 *
 * Passing an HTML element is better because at the time of writing (Sep 2022) Safari just fixed all
 * its a11y problems with CSS display:contents but it's still not widely used. Then, display:contents
 * should become the default behavior
 *
 * @see https://caniuse.com/css-display-contents
 *
 *
 * If you need custom control over the HTML attributes, you can
 * - use the `useGetAnalyticsAttributes` hook in React contexts
 * - use the `getOverrideHtmlAttributes` function in non-React contexts or old Class-based components
 * See the Analytics documentation for more details. (analytics.stories.mdx at the time of writing)
 *
 * @example <caption>Standard usage</caption>
 * <Analytics name="nameOfTheComponent">
 *   <div>Div</div> // will be augmented with some HTML attributes
 * </Analytics>
 * <Analytics name="nameOfTheComponent">
 *   <Component /> // will be wrapped in a <div style="display:contents">
 * </Analytics>
 * <Analytics name="nameOfTheComponent">
 *   <Component passHtmlAttributesToChildren /> // will be augmented with some HTML attributes
 * </Analytics>
 *
 * @example <caption>Redact some attributes</caption>
 * <Analytics name="nameOfTheComponent" htmlAttributesToRedact="data-test-id,data-cy">
 *   <div
 *     data-test-id="admin"
 *     data-cy"admin"
 * >
 *   The password of the DB is hidden!
 * </div>
 * </Analytics>

 * @example <caption>Redact just the text</caption>
 * <Analytics name="nameOfTheComponent" redactText>
 *   <div>The password of the DB is admin!</div>
 * </Analytics>
 *
 * @example <caption>Redact every sensitive data from the passed element (use it sparingly, it's better to redact only the needed attributes)</caption>
 * <Analytics name="nameOfTheComponent" {...REDACT_EVERYTHING}>
 *   <div data-test-id="admin">The password of the DB is admin!</div>
 * </Analytics>
 *
 */
export function Analytics(props: AnalyticsProps) {
  const isNotProduction = process.env.NODE_ENV !== 'production';
  const { children, name, passHtmlAttributesToChildren, ...analyticsOptions } =
    props;

  const { type, children: typedChildren } = detectChildrenType(children);

  const htmlAttributes = useGetAnalyticsAttributes(name, analyticsOptions);

  //  --------------------------------------------------
  //  EDGE CASES AND WARNINGS
  //  --------------------------------------------------
  if (type === 'noChildren') return null;

  // Handling the <Analytics><title>XXX</title></Analytics> edge case
  if (
    type === 'htmlElement' &&
    typedChildren.type === 'title' &&
    isNotProduction
  )
    warnIfTitleElementIsPassed(name);

  if (passHtmlAttributesToChildren && isNotProduction) {
    if (type === 'text') warnIfPassingHtmlAttributesToTexts(name);
    if (type === 'htmlElement') warnIfPassingHtmlAttributesToHtmlElements(name);
  }
  if (
    !passHtmlAttributesToChildren &&
    childrenAreKnownToAcceptHTMLAttributes(typedChildren)
  ) {
    warnIfWrappingChildrenAcceptingHtmlAttributes(name);
  }

  //  --------------------------------------------------
  //  STANDARD CASES
  //  --------------------------------------------------
  switch (getHtmlAttributesInjectionMode(type, passHtmlAttributesToChildren)) {
    case 'wrapInDiv':
      return (
        <div style={{ display: 'contents' }} {...htmlAttributes}>
          {typedChildren}
        </div>
      );

    case 'passHtmlAttributesToChildren':
    default:
      // @ts-expect-error TS 4.5.2 (used in the Console at the time of writing) does not support inferring
      // the correct type of children. TS 4.8 (which I tested) works fine.
      if (typedChildren.props) {
        const overrideHtmlAttributes = getOverrideHtmlAttributes(
          // @ts-expect-error TS 4.5.2 (used in the Console at the time of writing) does not support inferring
          // the correct type of children. TS 4.8 (which I tested) works fine.
          typedChildren.props,
          htmlAttributes
        );

        if (overrideHtmlAttributes.length) {
          const overrideError = new Error(
            `All the following attributes will be overridden: ${overrideHtmlAttributes} for the element with name "${name}"`
          );
          programmaticallyTraceError(overrideError);
        }
      }

      return React.cloneElement(
        // @ts-expect-error TS 4.5.2 (used in the Console at the time of writing) does not support inferring
        // the correct type of children. TS 4.8 (which I tested) works fine.
        typedChildren,
        htmlAttributes
      );
  }
}
