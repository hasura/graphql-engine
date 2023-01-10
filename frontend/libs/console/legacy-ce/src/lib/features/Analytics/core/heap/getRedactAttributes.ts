import type { RedactOptions, HtmlRedactAttributes } from './types';

/**
 * The options to use to redact every data from an HTML element.
 *
 * @see: the list of sensitive data in the DOM elements https://docs.google.com/document/d/1bVXpz0p1k2os7lmvSuwDzQDUoYwGQ9thOyckcuI6Hgk/edit#heading=h.c9iw8i7zh51x
 *
 * @deprecated It is meant for migrating from app-redaction to section-redaction, but must be removed
 * when all the sections have been migrated to surgically redact only the components that contain sensitive data.
 */
export const REDACT_EVERYTHING: RedactOptions = {
  redactText: true,
  htmlAttributesToRedact:
    'id,title,data-test,data-element,data-cy,data-testid,data-index-id,data-key,href,value,name,key,placeholder,for',
};

export function getRedactAttributes(
  options: RedactOptions
): HtmlRedactAttributes {
  const htmlAttributes: HtmlRedactAttributes = {};

  if (options.redactText) {
    htmlAttributes['data-heap-redact-text'] = true;
  }

  if (options.htmlAttributesToRedact) {
    htmlAttributes['data-heap-redact-attributes'] =
      options.htmlAttributesToRedact;
  }

  return htmlAttributes;
}
