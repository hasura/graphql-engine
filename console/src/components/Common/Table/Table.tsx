import React, { ReactNode, useEffect } from 'react';
import { TableProvider, useTable } from './hooks';
import styles from './Table.scss';

/**
 * In the future, this component can be extended to be able to have a table without
 * headers and also side-headers. Since CSS GRID is used here, CSS SUBGRID will make it
 * very easy to do (currently not supported in many browsers). Also some of the compoentnets
 * here depends of row and column keys to be set, we can provide default keys in those instances.
 */

type TableProps = {
  columnCount: number;
  rowCount: number;
};

const SideBarItem: React.FC<{
  onClick?: (value: any[]) => void;
  closeEdit?: (value: string) => void;
  renderItem?: (item: string) => ReactNode;
  tabIndex?: number;
  isLast: boolean;
  item: string;
}> = ({ item, onClick, isLast, renderItem, tabIndex }) => {
  const {
    setCurrentRow,
    setShowForm,
    data,
    inputValue,
    setInputValue,
    readonlyRows,
  } = useTable();

  const readonly = readonlyRows.includes(item);
  return (
    <div
      key={item}
      tabIndex={tabIndex}
      className={styles.sidebar_item}
      onClick={() => {
        if (!readonly) {
          setCurrentRow(item);
          setShowForm(true);
          onClick?.(data[item]);
        }
      }}
    >
      {isLast && (
        <input
          className="form_control"
          value={inputValue}
          placeholder="Enter new Role"
          onChange={e => setInputValue(e.target.value)}
        />
      )}
      <div className={`${isLast ? styles.hidden : ''}`}>
        {renderItem ? renderItem(item) : item}
      </div>
    </div>
  );
};

type InternalTableFormProps<T = any> = {
  children: (data: {
    currentData: T;
    currentColKey: string;
    currentRowKey: string;
    expandForm: () => void;
    collapseForm: () => void;
  }) => ReactNode;
};

export type TableFormProps<T = any> = Parameters<
  InternalTableFormProps<T>['children']
>[0];

export function TableForm<T = any>({ children }: InternalTableFormProps<T>) {
  const {
    currentRow,
    data,
    colKeys,
    setShowForm,
    showForm,
    inputValue,
    setInputValue,
    setCurrentRow,
    currentCol,
  } = useTable();
  const currentData = colKeys
    .slice(1)
    .reduce((a: Record<string, any>, b, i) => {
      a[b] = data[currentRow]?.[i];
      return a;
    }, {});
  return (
    <div className={`${showForm ? '' : styles.collapsed} ${styles.table_form}`}>
      {children?.({
        currentColKey: colKeys[currentCol + 1],
        currentData: currentData as T,
        currentRowKey: currentRow || inputValue,
        expandForm: () => setShowForm(true),
        collapseForm: () => {
          setShowForm(false);
          setInputValue('');
          setCurrentRow('---');
        },
      })}
    </div>
  );
}

export const TableContainer: React.FC<TableProps> = ({
  children,
  rowCount,
  columnCount,
}) => {
  return (
    <div
      className={styles.table}
      style={{
        gridTemplateColumns: `250px repeat(${
          columnCount - 1
        }, minmax(max-content, 1fr))`,
        gridTemplateRows: `repeat(${rowCount + 3}, max-content)`,
      }}
    >
      {children}
    </div>
  );
};

export const Table: React.FC<TableProps> = ({
  children,
  rowCount,
  columnCount,
}) => {
  return (
    <TableProvider>
      <TableContainer rowCount={rowCount} columnCount={columnCount}>
        {children}
      </TableContainer>
    </TableProvider>
  );
};

type TableRowProps<T = any> = {
  entries: T[];
  /* Used to handle onClick event on row */
  index: string;
  isSingleColumn?: boolean;
  readonly?: boolean;
  renderCol: (value: {
    data: T;
    columnKey: string;
    index: number;
  }) => ReactNode;
};

export function TableRow<T>({
  entries,
  renderCol,
  index,
  readonly = false,
  isSingleColumn: singleColumn = false,
}: TableRowProps<T>) {
  const {
    updateData,
    colKeys,
    setCurrentCol,
    setCurrentRow,
    currentRow,
    setShowForm,
    addReadOnlyRow: addReadOnlyColumn,
  } = useTable();
  useEffect(() => {
    updateData(index, entries);
    if (readonly) {
      addReadOnlyColumn(index);
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [entries]);
  const onClick = (col: number) => () => {
    if (!readonly) {
      setCurrentRow(index);
      setShowForm(true);
      setCurrentCol(col);
    }
  };
  return (
    <>
      {entries.map((data, i) => {
        return (
          <div
            className={`${
              currentRow === index
                ? styles.table_form_active
                : styles.justify_center
            } ${singleColumn ? styles.single_column : styles.justify_center}`}
            onClick={onClick(i)}
            key={i}
          >
            {renderCol({
              data,
              index: i,
              columnKey: colKeys[i + 1],
            })}
          </div>
        );
      })}
    </>
  );
}

type TableHeaderProps = {
  headers: string[];
  keys?: string[];
  onClick?: (value: string) => void;
};

export const TableHeader: React.FC<TableHeaderProps> = ({ headers, keys }) => {
  if ((keys?.length || 0) > 0 && keys?.length !== headers.length) {
    throw new Error('Keys, when specified, must be the same length as headers');
  }
  const { setColKeys, hasLegend } = useTable();

  useEffect(() => {
    setColKeys(keys?.length ? keys : headers);
  }, [keys, headers, setColKeys]);

  return (
    <>
      {headers.map((e, i) => (
        <div
          className={`${hasLegend ? '' : styles.top} ${styles.header_item}`}
          key={i}
        >
          {e}
        </div>
      ))}
    </>
  );
};

type TableSideBarProps = {
  items?: string[];
  renderItem?: (item: string) => ReactNode;
  onClick?: (value: any, index: number) => void;
  closeEdit?: () => void;
};

export const TableSideBar: React.FC<TableSideBarProps> = ({
  items,
  onClick,
  renderItem,
}) => {
  const { data, setRowKeys, rowKeys } = useTable();
  useEffect(() => {
    if (items) {
      setRowKeys(items);
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [items]);
  return (
    <>
      {rowKeys.map((item, i) => (
        <SideBarItem
          key={i}
          item={item}
          tabIndex={i}
          renderItem={renderItem}
          isLast={rowKeys.length === i + 1}
          onClick={() => onClick?.(data[item], i)}
        />
      ))}
    </>
  );
};

export const TableLegend: React.FC = ({ children }) => {
  const { setHasLegend } = useTable();
  useEffect(() => {
    setHasLegend(true);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);
  return <div className={styles.table_legend}>{children}</div>;
};
