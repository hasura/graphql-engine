import type { ReactNode } from 'react';

export function detectChildrenType(children: ReactNode) {
  if (children === undefined || children === null)
    return { type: 'noChildren', children } as const;

  if (
    typeof children === 'string' ||
    typeof children === 'number' ||
    typeof children === 'boolean'
  ) {
    return { type: 'text', children } as const;
  }

  // see: https://stackblitz.com/edit/react-ts-q6goee?file=App.tsx
  const childrenIsHtmlElement =
    'type' in children && typeof children.type === 'string';
  if (childrenIsHtmlElement) {
    return { type: 'htmlElement', children } as const;
  }

  return { type: 'reactComponent', children } as const;
}
