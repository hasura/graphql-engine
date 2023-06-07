import clsx from 'clsx';
import React from 'react';

export const Label: React.FC<{
  className?: string;
}> = ({ children, className }) => (
  <div
    className={clsx('pl-4 text-sm my-1 text-opacity-90 text-muted', className)}
  >
    {children}
  </div>
);
