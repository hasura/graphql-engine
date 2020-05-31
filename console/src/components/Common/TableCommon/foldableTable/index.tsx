/* eslint-disable no-param-reassign */
import React, { ComponentProps } from 'react';
import ReactTable from 'react-table';

import styles from '../Table.scss';

type FoldIconProps = {
  collapsed: boolean;
  name: string;
};
const FoldIcon: React.FC<FoldIconProps> = ({ collapsed, name }) => {
  let icon;
  let title;

  if (collapsed) {
    icon = 'fa-caret-right';
    title = name ? `Expand column "${name}"` : 'Expand column';
  } else {
    icon = 'fa-caret-left';
    title = 'Collapse column';
  }

  return <i className={`fa ${icon}`} title={title} />;
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

interface Column extends Record<string, any> {
  Header: React.ReactNode;
  accessor: string;
  id?: string;
  width: number;
}

export interface FoldableTableProps extends ComponentProps<typeof ReactTable> {
  onFoldChange: (a: Record<string, boolean>) => void;
  folded: Record<string, boolean>;
  foldableOriginalKey: number;
  columns: Column[];
}
class FoldableTable extends React.Component<FoldableTableProps> {
  static displayName = 'RTFoldableTable';

  onFold = (col: Column) => {
    if (!col || !col.id) return;

    const { onFoldChange } = this.props;
    const { id } = col;

    const newFold = { ...this.props.folded, [id]: !this.props.folded[id] };

    if (onFoldChange) {
      onFoldChange(newFold);
    }
  };

  copyOriginals = (column: Column) => {
    // Stop copy if the column already copied
    if (column.original_Header) return;

    Object.keys(foldedColumn).forEach(k => {
      const copiedKey = `${foldableOriginalKey}${k}`;

      if (k === 'Cell') {
        column[copiedKey] = column[k] ? column[k] : (c: Column) => c.value;
      } else column[copiedKey] = column[k];
    });

    // Copy sub Columns
    if (column.columns && !column.original_Columns) {
      column.original_Columns = column.columns;
    }

    // Copy Header
    if (!column.original_Header) {
      column.original_Header = column.Header;
    }
  };

  restoreToOriginal = (column: Column) => {
    Object.keys(foldedColumn).forEach(k => {
      // ignore header as handling by foldableHeaderRender
      if (k === 'Header') return;

      const copiedKey = `${foldableOriginalKey}${k}`;
      column[k] = column[copiedKey];
    });

    if (column.columns && column.original_Columns) {
      column.columns = column.original_Columns;
    }
  };

  applyFoldableForColumn = (column: Column) => {
    const collapsed = column.id && this.props.folded[column.id];
    if (collapsed) {
      if (column.columns) {
        column.columns = [foldedColumn];
        column.width = foldedColumn.width;
      } else {
        Object.assign(column, foldedColumn);
      }
    } else this.restoreToOriginal(column);
  };

  applyFoldableForColumns = (columns: Column[]) => {
    return columns.map((col, index) => {
      if (!col.foldable) return col;

      // If col don't have id then generate id based on index
      if (!col.id) {
        col.id = `col_${index}`;
      }

      this.copyOriginals(col);
      // Replace current header with internal header render.
      col.Header = () => (
        <FoldableHeader
          isFolded={!!(col.id && this.props.folded[col.id])}
          onFold={this.onFold}
          column={col}
        />
      );
      // apply foldable
      this.applyFoldableForColumn(col);

      // return the new column out
      return col;
    });
  };

  render() {
    const { columns: originalCols, ...rest } = this.props;
    const columns = this.applyFoldableForColumns([...originalCols]);

    return <ReactTable {...rest} columns={columns} />;
  }
}

export default FoldableTable;
