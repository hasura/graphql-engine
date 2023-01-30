import clsx from 'clsx';
import React from 'react';

export const Content: React.FC = ({ children }) => (
  <div
    className={clsx(
      'min-w-[220px] bg-white rounded-md py-2 shadow-lg',
      'group-data-[side=top]/content:animate-slideDownAndFade',
      'group-data-[side=right]/content:animate-slideLeftAndFade',
      'group-data-[side=bottom]/content:animate-slideUpAndFade',
      'group-data-[side=left]/content:animate-slideRightAndFade'
    )}
  >
    {children}
  </div>
);
