/* eslint-disable no-param-reassign */
import React, { ComponentProps, useCallback, useMemo } from 'react';
import ReactTable from 'react-table';

import styles from '../Table.scss';
import { exists } from '../../utils/jsUtils';

type FoldIconProps = {
  collapsed: boolean;
  name: string;
};
const FoldIcon: React.FC<FoldIconProps> = ({ collapsed, name }) => {
  if (collapsed) {
    return (
      <i
        className="fa fa-caret-right"
        title={name ? `Expand column "${name}"` : 'Expand column'}
      />
    );
  }

  return <i className="fa fa-caret-left" title="Collapse column" />;
};

type FoldableHeaderProps = {
  column: Column;
  isFolded: boolean;
  onFold(c: Column): void;
};
const FoldableHeader: React.FC<FoldableHeaderProps> = ({
  column,
  isFolded,
  onFold,
}) => {
  const onClick = (e: React.MouseEvent<HTMLDivElement>) => {
    e.stopPropagation();
    onFold(column);
  };

  return (
    <>
      <div className={styles.foldButton} onClick={onClick} role="button">
        <FoldIcon collapsed={isFolded} name={column.id || ''} />
      </div>
      {!isFolded && column.original_Header}
    </>
  );
};

const foldedColumn = {
  Cell: () => '',
  width: 22,
  headerClassName: 'collapsed',
  sortable: false,
  resizable: false,
  filterable: false,
};
const foldableOriginalKey = 'original_';

const copyOriginals = (column: Column & { id: string }) => {
  // Stop copy if the column already copied
  if (column.original_Header) return column;

  const newColumn = { ...column };

  Object.keys(foldedColumn).forEach(k => {
    const copiedKey = `${foldableOriginalKey}${k}`;

    if (k === 'Cell') {
      newColumn[copiedKey] = column[k] ? column[k] : (c: Column) => c.value;
    } else newColumn[copiedKey] = column[k];
  });

  // Copy sub Columns
  if (column.columns && !column.original_Columns) {
    newColumn.original_Columns = column.columns;
  }

  // Copy Header
  if (!column.original_Header) {
    newColumn.original_Header = column.Header;
  }
  return newColumn;
};

const applyFoldableForColumn = (
  column: Column,
  index: number,
  folded: Record<string, boolean>,
  onFold: (c: Column) => void
) => {
  if (!column.foldable) {
    return column;
  }

  const newColumn = copyOriginals({
    ...column,
    id: column.id || `col_${index}`,
  });

  const collapsed = folded[newColumn.id];

  newColumn.Header = () => (
    <FoldableHeader isFolded={collapsed} onFold={onFold} column={newColumn} />
  );
  if (collapsed) {
    if (column.columns) {
      newColumn.columns = [foldedColumn];
      newColumn.width = foldedColumn.width;
      return newColumn;
    }
    return { ...newColumn, ...foldedColumn };
  }
  return newColumn;
};

interface Column extends Record<string, any> {
  Header: React.ReactNode;
  accessor: string;
  id?: string;
  width: number;
  columns?: typeof foldedColumn[];
}

export interface FoldableTableProps extends ComponentProps<typeof ReactTable> {
  onFoldChange: (a: Record<string, boolean>) => void;
  folded: Record<string, boolean>;
  columns: Column[];
}

const FoldableTable: React.FC<FoldableTableProps> = ({
  onFoldChange,
  columns: originalCols,
  folded,
  ...rest
}) => {
  const onFold = useCallback(
    (col?: Column) => {
      if (!col || !col.id) return;

      if (onFoldChange) {
        onFoldChange({ ...folded, [col.id]: !folded[col.id] });
      }
    },
    [folded]
  );

  const columns = useMemo(
    () =>
      originalCols
        .filter(exists)
        .map((col, index) =>
          applyFoldableForColumn(col, index, folded, onFold)
        ),
    [originalCols, folded]
  );

  return <ReactTable {...rest} columns={columns} />;
};

FoldableTable.displayName = 'RTFoldableTable';

export default FoldableTable;
