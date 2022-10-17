import React, { useRef } from 'react';
import { TableColumn } from '@/dataSources';
import { isEmpty } from '@/components/Common/utils/jsUtils';
import { TableRow } from '../Common/Components/TableRow';
import { RowValues } from './DataTableRowItem.types';

export type DataTableRowItemProps = {
  column: TableColumn;
  onColumnUpdate: (columnName: string, rowValues: RowValues) => void;
  enumOptions: any;
  baseCopyRow: Record<string, unknown>;
  index: string;
  defaultValue?: any;
};

export const DataTableRowItem = ({
  column,
  onColumnUpdate,
  enumOptions,
  baseCopyRow,
  index,
  defaultValue = undefined,
}: DataTableRowItemProps) => {
  const refs = useRef<RowValues>({
    valueNode: null,
    nullNode: null,
    defaultNode: null,
    radioNode: null,
  });

  const {
    column_name: columnName,
    is_identity: isIdentity,
    column_default,
  } = column;

  const hasDefault = column_default && column_default.trim() !== '';

  const onChange = (e: React.ChangeEvent<HTMLInputElement>, val: unknown) => {
    const textValue = typeof val === 'string' ? val : e.target.value;

    const radioToSelectWhenEmpty =
      hasDefault || isIdentity
        ? refs?.current?.defaultNode
        : refs?.current?.nullNode;

    if (refs?.current?.radioNode) {
      refs.current.radioNode.checked = !!textValue.length;
    }

    if (radioToSelectWhenEmpty) {
      radioToSelectWhenEmpty.checked = !textValue.length;
    }

    if (typeof radioToSelectWhenEmpty?.checked !== 'undefined') {
      if (radioToSelectWhenEmpty) {
        radioToSelectWhenEmpty.checked = !textValue.length;
      }
    }

    onColumnUpdate(columnName, refs?.current);
  };

  const onFocus = (e: React.FocusEvent<HTMLInputElement, Element>) => {
    const textValue = e.target.value;
    if (isEmpty(textValue)) {
      const radioToSelectWhenEmpty = hasDefault
        ? refs?.current.defaultNode
        : refs?.current.nullNode;

      if (refs?.current?.radioNode) {
        refs.current.radioNode.checked = false;
      }

      if (radioToSelectWhenEmpty) {
        radioToSelectWhenEmpty.checked = true;
      }
    }
  };

  return (
    <TableRow
      column={column}
      setRef={(key, node) => {
        if (refs?.current) {
          refs.current[key] = node;
        }
      }}
      prevValue={defaultValue}
      enumOptions={enumOptions}
      clone={baseCopyRow}
      onChange={onChange}
      onFocus={onFocus}
      index={index}
    />
  );
};
