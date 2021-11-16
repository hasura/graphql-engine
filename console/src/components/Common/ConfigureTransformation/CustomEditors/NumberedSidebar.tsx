import React from 'react';
import KnowMore from '../../KnowMoreLink/KnowMore';
import { sidebarNumberStyles } from '../utils';

interface NumberedSidebarProps {
  number: string;
  title: string;
  url?: string;
  description?: string;
}

const NumberedSidebar: React.FC<NumberedSidebarProps> = ({
  number,
  title,
  url,
  description,
  children,
}) => {
  return (
    <>
      <div className={sidebarNumberStyles}>{number}</div>
      <div className="flex items-center mb-sm">
        <div>
          <label className="flex items-center block text-gray-600 font-medium">
            {title}
            {url ? <KnowMore url={url} /> : null}
          </label>
          {description ? (
            <p className="text-sm text-gray-600">{description}</p>
          ) : null}
        </div>
        {children}
      </div>
    </>
  );
};

export default NumberedSidebar;
