import React, { useMemo, useCallback } from 'react';
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
        <label key={col} htmlFor={col} className={styles.add_mar_right_mid}>
          <input
            type="checkbox"
            className={`${styles.cursorPointer} ${styles.add_mar_right_small}`}
            onChange={() => onChange(col)}
            checked={selectedColumns.includes(col)}
          />
          {col}
        </label>
      ))}
    </>
  );
};

export interface ColumnsSelectorProps extends Props {
  setSelected: (columns: string[]) => void;
}
export const ColumnsSelector: React.FC<ColumnsSelectorProps> = ({
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

  const editorContent = useCallback(
    () => (
      <ColumnsSelectorContent
        allColumns={allColumns}
        selectedColumns={selectedColumns}
        onChange={toggleSelect}
      />
    ),
    [allColumns, selectedColumns]
  );

  return (
    <div className={styles.padd_left}>
      <div className={styles.display_flex}>
        <h2 className={`${styles.subheading_text} ${styles.padd_bottom}`}>
          Columns
        </h2>
        <i className={styles.selectedColumnsLabel}>{selectedColumnsLabel}</i>
      </div>
      <ExpandableEditor
        className={styles.remove_margin_top}
        editorExpanded={editorContent}
        expandButtonText="Configure"
        collapseButtonText="Close"
        removeButtonText="Reset"
        removeFunc={removeFunc}
        property="columns-selector"
      />
    </div>
  );
};
