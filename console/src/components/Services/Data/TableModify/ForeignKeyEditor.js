import React, { useEffect } from 'react';
import { ordinalColSort } from '../utils';
import { setForeignKeys, saveForeignKeys, removeForeignKey } from './ModifyActions';
import { pgConfTypes, getForeignKeyConfig, getExistingFKConstraints } from '../Common/ReusableComponents/utils';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import ForeignKeySelector from '../Common/ReusableComponents/ForeignKeySelector';


const ForeignKeyEditor = ({
  tableSchema,
  currentSchema,
  allSchemas,
  dispatch,
  styles,
  fkModify,
}) => {
  const columns = tableSchema.columns.sort(ordinalColSort);
  const tableName = tableSchema.table_name;
  const orderedColumns = columns.map((c, i) => ({
    name: c.column_name,
    index: i
  }));
  const existingForeignKeys = getExistingFKConstraints(tableSchema, orderedColumns);
  existingForeignKeys.push({
    refTableName: '',
    onUpdate: 'restrict',
    onDelete: 'restrict',
    colMappings: [{ column: '', refColumn: '' }]
  });
  useEffect(
    () => {
      dispatch(setForeignKeys(existingForeignKeys))
    },
    []
  )
  const refTables = {};
  allSchemas.forEach(tableSchema => {
    refTables[tableSchema.table_name] = tableSchema.columns.map(
      c => c.column_name
    );
  });
  return fkModify.map((fk, i) => {
    const fkConfig = getForeignKeyConfig(fk, orderedColumns);
    const collapsedLabel = () => (
      <div>
        <div className="container-fluid">
          <div className="row">
            <h5 className={styles.padd_bottom}>
              <i> {fkConfig} </i>
              &nbsp;
            </h5>
          </div>
        </div>
      </div>
    );

    const isLast = i + 1 === fkModify.length;

    const expandedContent = () => (
      <ForeignKeySelector
        refTables={refTables}
        foreignKey={fk}
        index={i}
        service="modify-table"
        foreignKeys={fkModify}
        orderedColumns={orderedColumns}
        dispatch={dispatch}
        styles={styles}
        setForeignKeys={setForeignKeys}
      />
    );
    const expandButtonText = isLast ? 'Add a new foreign key' : 'Edit';
   
    const resetFk = () => {
      const newFks = [...fkModify];
      newFks[i] = existingForeignKeys[i];
      dispatch(setForeignKeys(newFks));
    };

    const collapseCallback = () => {
      if (isLast) {
        dispatch(setForeignKeys(existingForeignKeys));
      }
    }
    const collapseButtonText = isLast ? 'Cancel' : 'Close';
    let removeFk;

    if (!isLast) {
      removeFk = () => {
        let isOk = window.confirm('Are you sure?');
        if (isOk) {
          dispatch(removeForeignKey(i, tableSchema, orderedColumns));
        }
      }
    }

    let saveFk;
    if (fkConfig) {
      saveFk = () => dispatch(saveForeignKeys(i, tableSchema, orderedColumns));
    }

    return (
      <div key={`${i}`}>
        <ExpandableEditor
          editorExpanded={expandedContent}
          property={'add-fks'}
          ongoingRequest={'oola'}
          service="modify-table"
          removeFunc={removeFk}
          saveFunc={saveFk}
          collapsedLabel={collapsedLabel}
          collapseCallback={resetFk}
          collapseButtonText={collapseButtonText}
          expandButtonText={expandButtonText}
        />
      </div>
    )
  }) 
}

export default ForeignKeyEditor;