import React from 'react';
import { ChangeLevel } from '../types';

export const CountLabel: React.VFC<{
  count: number;
  type: ChangeLevel;
}> = props => {
  const { count, type } = props;

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
    default:
      break;
  }

  return (
    <div className={`rounded ${backgroundColor}`}>
      <span className={`font-bold ${textColor} mx-2`}>{count}</span>
    </div>
  );
};
