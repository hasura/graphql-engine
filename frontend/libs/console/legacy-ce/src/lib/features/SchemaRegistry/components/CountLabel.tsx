import React from 'react';
import { ChangeLevel } from '../types';
import { FaExclamationTriangle } from 'react-icons/fa';
import { IconTooltip } from '../../../new-components/Tooltip';

export const CountLabel: React.VFC<{
  count?: number;
  type: ChangeLevel;
}> = props => {
  const { count, type } = props;

  if (count === undefined) {
    return (
      <IconTooltip
        icon={<FaExclamationTriangle className="pr-2 text-xl text-secondary" />}
        message={
          'Could not compute changes with respect to previous schema for this role. A previous schema for this role might not exist or one of the GraphQL schemas could be erroneous.'
        }
      />
    );
  }

  let textColor = 'text-green-500';
  let backgroundColor = 'bg-green-100';

  switch (type) {
    case 'BREAKING': {
      textColor = 'text-red-500';
      backgroundColor = 'bg-red-100';
      break;
    }
    case 'DANGEROUS': {
      textColor = 'text-yellow-500';
      backgroundColor = 'bg-yellow-100';
      break;
    }
    case 'NON_BREAKING': {
      textColor = 'text-green-500';
      backgroundColor = 'bg-green-100';
      break;
    }
    case 'TOTAL': {
      textColor = 'text-black';
      backgroundColor = '';
      break;
    }
    default:
      break;
  }

  return (
    <div className={`rounded ${backgroundColor}`}>
      <span className={`font-bold ${textColor} mx-2`}>{count}</span>
    </div>
  );
};
