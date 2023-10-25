import React from 'react';
import { CountLabel } from './CountLabel';
import { SchemaChange } from '../types';
import { CapitalizeFirstLetter } from '../utils';

export const SchemaRow: React.VFC<{
  role: string;
  changes?: SchemaChange[];
}> = props => {
  const { role, changes } = props;

  const countBreakingChanges = changes?.filter(
    c => c.criticality.level === 'BREAKING'
  )?.length;
  const countDangerousChanges = changes?.filter(
    c => c.criticality.level === 'DANGEROUS'
  )?.length;
  const countSafeChanges = changes?.filter(
    c => c.criticality.level === 'NON_BREAKING'
  )?.length;
  const totalCount =
    (countBreakingChanges || 0) +
    (countDangerousChanges || 0) +
    (countSafeChanges || 0);
  return (
    <div className="flex mt-8 px-4 py-2 w-full">
      <div className="flex text-base  justify-between w-[15%]">
        <span className="text-md font-bold bg-gray-100 rounded p-1">
          {CapitalizeFirstLetter(role)}
        </span>
      </div>
      <div className="flex text-base items-center justify-around w-[30%]">
        {changes ? (
          <>
            <CountLabel count={countBreakingChanges || 0} type="BREAKING" />
            <CountLabel count={countDangerousChanges || 0} type="DANGEROUS" />
            <CountLabel count={countSafeChanges || 0} type="NON_BREAKING" />
          </>
        ) : (
          <>
            <CountLabel count={countBreakingChanges} type="BREAKING" />
            <CountLabel count={countDangerousChanges} type="DANGEROUS" />
            <CountLabel count={countSafeChanges} type="NON_BREAKING" />
          </>
        )}
      </div>
      <div className="flex text-base items-center justify-around w-[55%]">
        <div className="font-bold text-xl mx-2">{totalCount}</div>
      </div>
    </div>
  );
};
