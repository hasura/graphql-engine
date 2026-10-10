import type { ReactNode } from 'react';

// Components known to accept and forward arbitrary HTML attributes, identified
// by their `displayName` to avoid a circular dependency on @hasura/shared/ui
// (which itself depends on @hasura/shared/analytics).
const KNOWN_HTML_ATTRIBUTES_ACCEPTING_DISPLAY_NAMES = ['Button'];

/**
 * Detects if the children are one of the Console's components that accepts HTML attributes.
 * The goal is to add the most common cases here after detecting them during the code reviews.
 *
 * ATTENTION: This is manually maintained!
 */
export function childrenAreKnownToAcceptHTMLAttributes(children: ReactNode) {
  if (children === undefined || children === null) return false;

  if (
    typeof children === 'string' ||
    typeof children === 'number' ||
    typeof children === 'bigint' ||
    typeof children === 'boolean'
  ) {
    return false;
  }

  if (!('type' in children)) return false;

  // children is a React Component
  const componentType = children.type as { displayName?: string };
  if (
    typeof componentType === 'object' &&
    componentType.displayName &&
    KNOWN_HTML_ATTRIBUTES_ACCEPTING_DISPLAY_NAMES.includes(
      componentType.displayName,
    )
  ) {
    return true;
  }

  return false;
}
