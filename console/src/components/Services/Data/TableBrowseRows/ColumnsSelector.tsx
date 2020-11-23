import React, { useMemo } from 'react';
import styles from '../../../Common/FilterQuery/FilterQuery.scss';
import CollapsibleToggle from '../../../Common/CollapsibleToggle/CollapsibleToggle';

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
    <div className={styles.displayFlexContainer} style={{ flexWrap: 'wrap' }}>
      {allColumns.map(col => (
        <label
          key={col}
          htmlFor={col}
          className={`${styles.add_mar_right_mid} ${styles.add_mar_top_small} ${styles.cursorPointer} ${styles.displayFlexContainer}`}
          onClick={() => onChange(col)}
        >
          <input
            type="checkbox"
            className={styles.cursorPointer}
            style={{ margin: '3px 4px 0px 0px' }}
            checked={selectedColumns.includes(col)}
            readOnly
          />
          {col}
        </label>
      ))}
    </div>
  );
};

export interface ColumnsSelectorProps extends Props {
  setSelected: (columns: string[]) => void;
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
}
export const ColumnsSelector: React.FC<ColumnsSelectorProps> = ({
  allColumns,
  selectedColumns,
  setSelected,
  isOpen,
  setIsOpen,
}) => {
  const selectedColumnsLabel = useMemo(() => {
    if (!allColumns.length) return '';
    if (!selectedColumns.length) return 'none selected';
    return allColumns.length === selectedColumns.length
      ? 'all selected'
      : selectedColumns.join(', ');
  }, [allColumns, selectedColumns]);

  const toggleSelect = (colName: string) => {
    if (selectedColumns.includes(colName)) {
      setSelected(selectedColumns.filter(c => c !== colName));
    } else {
      setSelected([...selectedColumns, colName]);
    }
  };

  return (
    <CollapsibleToggle
      title={
        <div className={styles.display_flex}>
          <h2 className={`${styles.subheading_text} ${styles.padd_bottom}`}>
            Columns
          </h2>
          <i className={styles.summaryLabel}>{selectedColumnsLabel}</i>
        </div>
      }
      isOpen={isOpen}
      toggleHandler={() => setIsOpen(prev => !prev)}
      className={styles.add_pad_bottom}
    >
      <ColumnsSelectorContent
        allColumns={allColumns}
        selectedColumns={selectedColumns}
        onChange={toggleSelect}
      />
    </CollapsibleToggle>
  );
};
