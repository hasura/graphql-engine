import { useState } from 'react';
import { TrackableTable } from '@hasura/metadata/api';

export const useCheckRows = (data: TrackableTable[]) => {
  const [checkedIds, setCheckedIds] = useState<string[]>([]);

  // Derived statuses
  const allChecked = data.length > 0 && checkedIds.length === data.length;

  // Input field determinate status
  const partialSelection =
    checkedIds.length > 0 && checkedIds.length < data.length;
  const inputStatus: 'indeterminate' | 'determinate' = partialSelection
    ? 'indeterminate'
    : 'determinate';

  const onCheck = (id: string) => {
    setCheckedIds((prev) =>
      prev.includes(id) ? prev.filter((item) => item !== id) : [...prev, id],
    );
  };

  const toggleAll = () => {
    if (allChecked) {
      setCheckedIds([]);
    } else {
      setCheckedIds(data.map((item) => item.id));
    }
  };

  const reset = () => {
    setCheckedIds([]);
  };

  return {
    checkedIds,
    allChecked,
    inputStatus,
    reset,
    onCheck,
    toggleAll,
  };
};
