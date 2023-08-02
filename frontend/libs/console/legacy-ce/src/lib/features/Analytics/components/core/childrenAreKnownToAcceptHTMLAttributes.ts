import type { ReactNode } from 'react';
import { Button } from '../../../../new-components/Button';

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
    typeof children === 'boolean'
  ) {
    return false;
  }

  if (!('type' in children)) return false;

  // children is a React Component

  if (children.type === Button) return true;

  return false;
}
