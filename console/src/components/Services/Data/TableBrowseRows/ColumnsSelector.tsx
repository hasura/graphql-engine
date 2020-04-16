import React, { useMemo } from 'react';
import styles from '../../../Common/TableCommon/Table.scss';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';

interface Props {
  allColumns: string[];
  selectedColumns: string[];
}

type ColumnsSelectorContentProps = {
  onChange: (col: string) => void;
} & Props;
const ColumnsSelectorContent = ({
  allColumns = [],
  selectedColumns = [],
  onChange,
}: ColumnsSelectorContentProps) => {
  return (
    <>
      {allColumns.map(col => (
        <label htmlFor={col} style={{ paddingRight: 10 }}>
          <input
            type="checkbox"
            className={`${styles.cursorPointer}`}
            onChange={() => onChange(col)}
            checked={selectedColumns.includes(col)}
            style={{ marginRight: 5 }}
          />
          {col}
        </label>
      ))}
    </>
  );
};

export interface ColumnSelectorProps extends Props {
  setSelected: (columns: string[]) => void;
}
export const ColumnsSelector: React.FC<ColumnSelectorProps> = ({
  allColumns,
  selectedColumns,
  setSelected,
}) => {
  const toggleSelect = (colName: string) => {
    if (selectedColumns.includes(colName)) {
      setSelected(selectedColumns.filter(c => c !== colName));
    } else {
      setSelected([...selectedColumns, colName]);
    }
  };

  const selectedColumnsLabel = useMemo(() => {
    if (!allColumns.length) return '';
    return allColumns.length === selectedColumns.length
      ? 'all selected'
      : selectedColumns.join(', ');
  }, [allColumns, selectedColumns]);

  const removeFunc = useMemo(() => {
    if (!allColumns.length) return undefined;
    return allColumns.length !== selectedColumns.length
      ? () => setSelected(allColumns)
      : undefined;
  }, [allColumns, selectedColumns]);

  return (
    <div className={styles.padd_left}>
      <div style={{ display: 'flex', alignItems: 'center' }}>
        <h2 className={`${styles.subheading_text} ${styles.padd_bottom}`}>
          Columns
        </h2>
        <i style={{ paddingBottom: 9, paddingLeft: 10 }}>
          {selectedColumnsLabel}
        </i>
      </div>
      <ExpandableEditor
        className={styles.remove_margin_top}
        editorExpanded={() => (
          <ColumnsSelectorContent
            allColumns={allColumns}
            selectedColumns={selectedColumns}
            onChange={toggleSelect}
          />
        )}
        expandButtonText="Configure"
        collapseButtonText="Close"
        removeButtonText="Reset"
        removeFunc={removeFunc}
        property="columns-selector"
      />
    </div>
  );
};
