import React from 'react';
import { CountLabel } from './CountLabel';
import { SchemaChange } from '../types';
import { FaChevronRight } from 'react-icons/fa';

export const SchemaRow: React.VFC<{
  role: string;
  changes?: SchemaChange[];
  onClick?: VoidFunction;
}> = props => {
  const { role, changes, onClick } = props;

  const countBreakingChanges = changes?.filter(
    c => c.criticality.level === 'BREAKING'
  )?.length;
  const countDangerousChanges = changes?.filter(
    c => c.criticality.level === 'DANGEROUS'
  )?.length;
  const countSafeChanges = changes?.filter(
    c => c.criticality.level === 'NON_BREAKING'
  )?.length;
  return (
    <div className="w-full flex my-2">
      <div className="flex text-base w-[72%] justify-start">
        <span className="text-sm font-bold bg-gray-100 p-1 rounded">
          {role}
        </span>
      </div>
      <div className="flex text-base w-[28%] justify-between">
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
      {onClick && (
        <div
          className="flex text-base w-[2%] justify-end mt-[6px]"
          role="button"
          onClick={onClick}
        >
          <FaChevronRight />
        </div>
      )}
      {!onClick && <div className="flex w-[4%] justify-end"></div>}
    </div>
  );
};
