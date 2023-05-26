import { useState } from 'react';
import produce from 'immer';
import React from 'react';
import { AiFillCaretDown } from 'react-icons/ai';
import { DropdownMenu } from '../../../../new-components/DropdownMenu';

export const useCheckRows = <T,>(
  data: (T & { id: string })[],
  allData: (T & { id: string })[]
) => {
  const [checkedIds, setCheckedIds] = useState<string[]>([]);

  const checkboxRef = React.useRef<HTMLInputElement>(null);

  // Derived statuses
  const allChecked =
    (data.length > 0 && checkedIds.length === data.length) ||
    (allData.length > 0 && checkedIds.length === allData.length);

  // Input field determinate status
  const partialSelection =
    checkedIds.length > 0 && checkedIds.length < data.length;

  const inputStatus: 'indeterminate' | 'determinate' = partialSelection
    ? 'indeterminate'
    : 'determinate';

  const onCheck = (id: string) => {
    setCheckedIds(prev =>
      produce(prev, draft => {
        if (draft.includes(id)) {
          const i = draft.indexOf(id);
          draft.splice(i, 1);
        } else {
          draft.push(id);
        }
      })
    );
  };

  const toggleAll = (useOriginalList?: boolean) => {
    if (useOriginalList) {
      setCheckedIds(allData.map(item => item.id));
      return;
    }

    if (allChecked) {
      setCheckedIds([]);
    } else {
      setCheckedIds(data.map(item => item.id));
    }
  };

  const reset = () => {
    setCheckedIds([]);
  };

  React.useEffect(() => {
    if (!checkboxRef.current) return;
    checkboxRef.current.indeterminate = inputStatus === 'indeterminate';
  }, [inputStatus]);

  const checkAllElement = () => (
    <div className="flex items-center gap-2">
      <input
        ref={checkboxRef}
        type="checkbox"
        className="cursor-pointer
              rounded border shadow-sm border-gray-400 hover:border-gray-500 focus:ring-yellow-400"
        checked={allChecked}
        onChange={() => {
          toggleAll();
        }}
      />
      <DropdownMenu
        items={[
          [
            <span className="py-1.5" onClick={() => toggleAll(true)}>
              All {allData.length} items
            </span>,
          ],
        ]}
      >
        <AiFillCaretDown />
      </DropdownMenu>
    </div>
  );

  return {
    checkedIds,
    allChecked,
    reset,
    onCheck,
    toggleAll,
    checkboxRef,
    checkAllElement,
  };
};
