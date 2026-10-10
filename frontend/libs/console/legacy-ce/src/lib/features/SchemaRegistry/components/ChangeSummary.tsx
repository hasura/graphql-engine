import * as React from 'react';
import { Flex } from '@radix-ui/themes';
import { RoleBasedSchema } from '../types';

export const ChangeSummary: React.FC<{
  changes: RoleBasedSchema['changes'];
}> = (props) => {
  const { changes } = props;

  if (!changes) {
    return <span>Unknown</span>;
  }

  const numBreakingChanges = changes.filter(
    (c) => c.criticality.level === 'BREAKING',
  ).length;
  const numDangerousChanges = changes.filter(
    (c) => c.criticality.level === 'DANGEROUS',
  ).length;
  const numSafeChanges = changes.filter(
    (c) => c.criticality.level === 'NON_BREAKING',
  ).length;

  if (
    numBreakingChanges === 0 &&
    numDangerousChanges === 0 &&
    numSafeChanges === 0
  ) {
    return <span>No changes detected</span>;
  }

  return (
    <Flex direction="row" justify="between" className="w-[28%]">
      <div className="flex-col">
        <Flex className="text-red-600 text-2xl font-bold">
          {numBreakingChanges}
        </Flex>
        <span>Breaking</span>
      </div>
      <div className="flex-col">
        <Flex className="text-red-800 text-2xl font-bold">
          {numDangerousChanges}
        </Flex>
        <span>Dangerous</span>
      </div>
      <div className="flex-col">
        <Flex className="text-green-600 text-2xl font-bold">
          {numSafeChanges}
        </Flex>
        <span>Safe</span>
      </div>
    </Flex>
  );
};
