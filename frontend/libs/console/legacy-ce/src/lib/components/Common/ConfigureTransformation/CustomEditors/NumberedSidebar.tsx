import React from 'react';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';
import { sidebarNumberStyles } from '../utils';

interface NumberedSidebarProps {
  title: string;
  description?: string | JSX.Element;
  number?: string;
  url?: string;
}

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
      <label className="flex items-center block text-gray-600 font-medium">
        {title}
        {url ? <LearnMoreLink href={url} /> : null}
      </label>
      <div className="flex items-center mb-sm">
        <div>
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
