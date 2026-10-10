import React from 'react';
import { Flex } from '@radix-ui/themes';
import { CountLabel } from './CountLabel';
import { SchemaChange } from '../types';
import { CapitalizeFirstLetter } from '../utils';

export const SchemaRow: React.FC<{
  role: string;
  changes?: SchemaChange[];
}> = (props) => {
  const { role, changes } = props;

  const countBreakingChanges = changes?.filter(
    (c) => c.criticality.level === 'BREAKING',
  )?.length;
  const countDangerousChanges = changes?.filter(
    (c) => c.criticality.level === 'DANGEROUS',
  )?.length;
  const countSafeChanges = changes?.filter(
    (c) => c.criticality.level === 'NON_BREAKING',
  )?.length;
  const totalCount =
    (countBreakingChanges || 0) +
    (countDangerousChanges || 0) +
    (countSafeChanges || 0);
  return (
    <Flex className="mt-8 px-4 py-2 w-full">
      <Flex justify="between" className="text-base w-[15%]">
        <span className="text-md font-bold bg-gray-100 rounded p-1">
          {CapitalizeFirstLetter(role)}
        </span>
      </Flex>
      <Flex align="center" className="text-base justify-around w-[30%]">
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
      </Flex>
      <Flex align="center" className="text-base justify-around w-[55%]">
        <div className="font-bold text-xl mx-2">{totalCount}</div>
      </Flex>
    </Flex>
  );
};
