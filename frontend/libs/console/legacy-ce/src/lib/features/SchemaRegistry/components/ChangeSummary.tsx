import * as React from 'react';
import { RoleBasedSchema } from '../types';

export const ChangeSummary: React.VFC<{
  changes: RoleBasedSchema['changes'];
}> = props => {
  const { changes } = props;

  if (!changes) {
    return <span>Unknown</span>;
  }

  const numBreakingChanges = changes.filter(
    c => c.criticality.level === 'BREAKING'
  ).length;
  const numDangerousChanges = changes.filter(
    c => c.criticality.level === 'DANGEROUS'
  ).length;
  const numSafeChanges = changes.filter(
    c => c.criticality.level === 'NON_BREAKING'
  ).length;

  if (
    numBreakingChanges === 0 &&
    numDangerousChanges === 0 &&
    numSafeChanges === 0
  ) {
    return <span>No changes detected</span>;
  }

  return (
    <div className="flex flex-row justify-between w-[28%]">
      <div className="flex-col">
        <div className="flex text-red-600 text-2xl font-bold">
          {numBreakingChanges}
        </div>
        <span>Breaking</span>
      </div>
      <div className="flex-col">
        <div className="flex text-red-800 text-2xl font-bold">
          {numDangerousChanges}
        </div>
        <span>Dangerous</span>
      </div>
      <div className="flex-col">
        <div className="flex text-green-600 text-2xl font-bold">
          {numSafeChanges}
        </div>
        <span>Safe</span>
      </div>
    </div>
  );
};
