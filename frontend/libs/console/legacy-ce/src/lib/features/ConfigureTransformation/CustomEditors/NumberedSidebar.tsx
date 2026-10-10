import React, { type JSX } from 'react';
import { FieldLabel, Text } from '@hasura/shared/ui';

interface NumberedSidebarProps {
  title: string;
  description?: string | JSX.Element;
  number?: string;
  url?: string;
  children?: React.ReactNode;
}

// Radix tokens so the step badge follows light/dark mode. The background must
// stay opaque (solid gray-2, not an alpha step) because the badge sits on top of
// the parent's `border-l` timeline and has to hide it.
const sidebarNumberStyles =
  '-mb-8 -ml-14 bg-(--gray-2) text-(--gray-12) text-sm font-medium border border-(--gray-a7) rounded-full flex items-center justify-center w-8 h-8';

const NumberedSidebar: React.FC<NumberedSidebarProps> = ({
  title,
  description,
  number,
  url,
  children,
}) => {
  return (
    <>
      {number ? <div className={sidebarNumberStyles}>{number}</div> : null}
      <FieldLabel label={title} learnMoreLink={url} />
      <div className="mb-2">
        <div className="mb-2">
          <Text size="1">
            {description ? (
              <p className="text-sm text-(--gray-11)">{description}</p>
            ) : null}
          </Text>
        </div>
        {children}
      </div>
    </>
  );
};

export default NumberedSidebar;
