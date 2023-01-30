import clsx from 'clsx';
import React from 'react';
import { FaChevronDown } from 'react-icons/fa';
import { Operation } from '../OASGeneratorModal/types';

export interface OasGeneratorMoreInfoProps {
  operation: Operation;
}

export const OasGeneratorMoreInfo: React.FC<
  OasGeneratorMoreInfoProps
> = props => {
  const { operation } = props;
  const [isExpanded, setExpanded] = React.useState(false);
  return (
    <div>
      <div
        className="flex justify-between cursor-pointer"
        onClick={() => setExpanded(!isExpanded)}
      >
        <div>{operation.path}</div>
        <div>
          More info{' '}
          <FaChevronDown
            className={clsx(
              isExpanded ? 'rotate-180' : '',
              'transition-all duration-300 ease-in-out'
            )}
          />
        </div>
      </div>
      <div
        className={clsx(
          'whitespace-normal',
          isExpanded ? 'h-auto pt-4' : 'h-0 pt-0',
          'overflow-hidden transition-all duration-300 ease-in-out'
        )}
      >
        {operation.description.trim() ??
          'No description available for this endpoint'}
      </div>
    </div>
  );
};
