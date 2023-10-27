import { useState } from 'react';
import produce from 'immer';
import React from 'react';
import { AiFillCaretDown } from 'react-icons/ai';
import { DropdownMenu } from '../../../../new-components/DropdownMenu';
import { FaFilter } from 'react-icons/fa';
import { BsCheck2All } from 'react-icons/bs';

export const useCheckRows = <T,>(
  data: (T & { id: string })[],
  filteredData: (T & { id: string })[],
  allData: (T & { id: string })[]
) => {
  const [checkedIds, setCheckedIds] = useState<string[]>([]);

  const checkboxRef = React.useRef<HTMLInputElement>(null);

  // Derived statuses
  const allChecked =
    (data.length > 0 && checkedIds.length === data.length) ||
    (allData.length > 0 && checkedIds.length === allData.length) ||
    (filteredData.length > 0 && checkedIds.length === filteredData.length);

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

  const toggleAll = (props?: {
    useOriginalList?: boolean;
    useAllFilteredList?: boolean;
  }) => {
    const { useOriginalList, useAllFilteredList } = props ?? {};

    if (useOriginalList) {
      setCheckedIds(allData.map(item => item.id));
      return;
    }

    if (useAllFilteredList) {
      setCheckedIds(filteredData.map(item => item.id));
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
            <div
              className="py-1.5 gap-2 flex items-center"
              onClick={() => toggleAll({ useAllFilteredList: true })}
            >
              <FaFilter /> All {filteredData.length} results (filtered)
            </div>,
          ],
          [
            <div
              className="py-1.5 gap-2 flex items-center"
              onClick={() => toggleAll({ useOriginalList: true })}
            >
              <BsCheck2All /> All {allData.length} items
            </div>,
          ],
        ]}
      >
        <AiFillCaretDown className="cursor-pointer" />
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
