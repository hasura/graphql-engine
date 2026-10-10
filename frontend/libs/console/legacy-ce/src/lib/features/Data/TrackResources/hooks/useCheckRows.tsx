import React, { useState } from 'react';
import { AiFillCaretDown } from 'react-icons/ai';
import {
  Checkbox,
  CheckedState,
  DropdownMenu,
  IconButton,
} from '@hasura/shared/ui';
import { FaFilter } from 'react-icons/fa';
import { BsCheck2All } from 'react-icons/bs';
import { Flex } from '@radix-ui/themes';

export const useCheckRows = <T,>(
  data: (T & { id: string })[],
  filteredData: (T & { id: string })[],
  allData: (T & { id: string })[],
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

  const inputStatus: CheckedState = partialSelection
    ? 'indeterminate'
    : checkedIds.length > 0;

  const onCheck = (id: string) => {
    setCheckedIds((prev) =>
      prev.includes(id) ? prev.filter((item) => item !== id) : [...prev, id],
    );
  };

  const toggleAll = (props?: {
    useOriginalList?: boolean;
    useAllFilteredList?: boolean;
  }) => {
    const { useOriginalList, useAllFilteredList } = props ?? {};

    if (useOriginalList) {
      setCheckedIds(allData.map((item) => item.id));
      return;
    }

    if (useAllFilteredList) {
      setCheckedIds(filteredData.map((item) => item.id));
      return;
    }

    if (allChecked) {
      setCheckedIds([]);
    } else {
      setCheckedIds(data.map((item) => item.id));
    }
  };

  const reset = () => {
    setCheckedIds([]);
  };

  const checkAllElement = () => (
    <Flex align="center" gap="2">
      <Checkbox
        value={inputStatus}
        onChange={() => {
          toggleAll();
        }}
      />
      {checkedIds.length < data.length && (
        <DropdownMenu.Root
          items={[
            <DropdownMenu.Item
              key="use-filtered"
              onSelect={() => toggleAll({ useAllFilteredList: true })}
            >
              <Flex key="filtered" gap="2" align="center" className="py-1.5">
                <FaFilter /> All {filteredData.length} results (filtered)
              </Flex>
            </DropdownMenu.Item>,
            <DropdownMenu.Item
              key="use-all"
              onSelect={() => toggleAll({ useOriginalList: true })}
            >
              <Flex key="all" gap="2" align="center" className="py-1.5">
                <BsCheck2All /> All {allData.length} items
              </Flex>
            </DropdownMenu.Item>,
          ]}
        >
          <IconButton variant="ghost" radius="full">
            <AiFillCaretDown className="cursor-pointer" />
          </IconButton>
        </DropdownMenu.Root>
      )}
    </Flex>
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
