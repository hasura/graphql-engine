import clsx from 'clsx';
import React from 'react';
import { HeightOptions } from '../Root';
const assignMaxHeight = (h: HeightOptions) => {
  switch (h) {
    case '50vh':
      return 'max-h-[50vh]';
    case '75vh':
      return 'max-h-[75vh]';
    case '85vh':
      return 'max-h-[85vh]';
    case '100vh':
      return 'max-h-[100vh]';
    default:
      return 'max-h-[65vh]';
  }
};
export const Content: React.FC<{ maxHeight: HeightOptions }> = ({
  maxHeight = '65vh',
  children,
}) => {
  return (
    <div
      className={clsx(
        'min-w-[220px] bg-white rounded-md py-2 shadow-lg',
        'group-data-[side=top]/content:animate-slideDownAndFade',
        'group-data-[side=right]/content:animate-slideLeftAndFade',
        'group-data-[side=bottom]/content:animate-slideUpAndFade',
        'group-data-[side=left]/content:animate-slideRightAndFade',
        'overflow-y-auto',
        assignMaxHeight(maxHeight)
      )}
    >
      {children}
    </div>
  );
};
