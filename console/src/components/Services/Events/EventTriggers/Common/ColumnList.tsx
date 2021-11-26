import React from 'react';
import { QualifiedTable } from '@/metadata/types';
import styles from '../TableCommon/EventTable.scss';
import { ETOperationColumn } from '../../types';

type ColumnListProps = {
  operationColumns: ETOperationColumn[];
  table: QualifiedTable;
  handleOperationsColumnsChange: (oc: ETOperationColumn[]) => void;
};

const ColumnList: React.FC<ColumnListProps> = props => {
  const { operationColumns, table, handleOperationsColumnsChange } = props;

  if (!table.name) {
    return <i>Select a table first to get column list</i>;
  }

  const handleToggleColumn = (opCol: ETOperationColumn) => {
    const newCols = operationColumns.map(o => {
      return {
        ...o,
        enabled: o.name === opCol.name ? !o.enabled : o.enabled,
      };
    });
    handleOperationsColumnsChange(newCols);
  };

  return (
    <>
      {operationColumns.map(opCol => (
        <div
          key={opCol.name}
          className={`${styles.padd_remove} ${styles.columnListElement}`}
        >
          <div className="checkbox ">
            <label className={styles.cursorPointer}>
              <input
                type="checkbox"
                checked={opCol.enabled}
                onChange={() => handleToggleColumn(opCol)}
                className={`${styles.cursorPointer} legacy-input-fix`}
              />
              {opCol.name}
            </label>
          </div>
        </div>
      ))}
    </>
  );
};

export default ColumnList;
