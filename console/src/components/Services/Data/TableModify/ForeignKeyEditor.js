import React, { useEffect } from 'react';
import { ordinalColSort } from '../utils';
import {
  setForeignKeys,
  saveForeignKeys,
  removeForeignKey,
} from './ModifyActions';
import {
  getForeignKeyConfig,
  getExistingFKConstraints,
} from '../Common/ReusableComponents/utils';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import ForeignKeySelector from '../Common/ReusableComponents/ForeignKeySelector';

import styles from './ModifyTable.scss';

const ForeignKeyEditor = ({ tableSchema, allSchemas, dispatch, fkModify }) => {
  const columns = tableSchema.columns.sort(ordinalColSort);

  // columns in the right order with their indices
  const orderedColumns = columns.map((c, i) => ({
    name: c.column_name,
    index: i,
  }));

  // restructure the existing foreign keys and add it to fkModify (for easy processing)
  const existingForeignKeys = getExistingFKConstraints(
    tableSchema,
    orderedColumns
  );
  existingForeignKeys.push({
    refTableName: '',
    onUpdate: 'restrict',
    onDelete: 'restrict',
    colMappings: [{ column: '', refColumn: '' }],
  });
  useEffect(() => {
    dispatch(setForeignKeys(existingForeignKeys));
  }, []);

  // Generate a list of reference tables and their columns
  const refTables = {};
  allSchemas.forEach(ts => {
    refTables[ts.table_name] = ts.columns.map(c => c.column_name);
  });
  const numFks = fkModify.length;

  // Map the foreign keys in the fkModify state and render
  return fkModify.map((fk, i) => {
    // FK config (example: (a, b) -> refTable(c, d))
    const fkConfig = getForeignKeyConfig(fk, orderedColumns);

    const isLast = i + 1 === numFks;

    // Label to show next to the 'Edit' button (the FK configuration)
    const collapsedLabelText =
      isLast && numFks === 1 ? 'No foreign keys' : <b>{fkConfig}</b>;
    const collapsedLabel = () => (
      <div>
        <div className="container-fluid">
          <div className="row">
            <h5 className={styles.padd_bottom}>
              {collapsedLabelText}
              &nbsp;
            </h5>
          </div>
        </div>
      </div>
    );

    // The content when the editor is expanded
    const expandedContent = () => (
      <ForeignKeySelector
        refTables={refTables}
        foreignKey={fk}
        index={i}
        service="modify-table"
        foreignKeys={fkModify}
        orderedColumns={orderedColumns}
        dispatch={dispatch}
        setForeignKeys={setForeignKeys}
      />
    );

    // The collapse button text when the editor is collapsed
    let expandButtonText = 'Edit';
    if (isLast) {
      expandButtonText = numFks === 1 ? 'Add' : 'Add a new foreign key';
    }

    // label next to the button when the editor is expanded
    const expandedLabel = () => {
      if (isLast) return null;
      const existingFkConfig = getForeignKeyConfig(
        existingForeignKeys[i],
        orderedColumns
      );
      return (
        <h5 className={styles.padd_bottom}>
          <b>{existingFkConfig}</b>
        </h5>
      );
    };

    // If the user made some changes and collapses the editor, the changes are lost
    const resetFk = () => {
      const newFks = [...fkModify];
      newFks[i] = existingForeignKeys[i];
      dispatch(setForeignKeys(newFks));
    };

    const collapseButtonText = isLast ? 'Cancel' : 'Close';

    // Function to remove FK (is undefined for the last FK)
    let removeFk;
    if (!isLast) {
      removeFk = () => {
        const isOk = window.confirm('Are you sure?');
        if (isOk) {
          dispatch(removeForeignKey(i, tableSchema, orderedColumns));
        }
      };
    }

    // Function to save the FK
    let saveFk;
    if (fkConfig) {
      saveFk = () => {
        dispatch(saveForeignKeys(i, tableSchema, orderedColumns));
      };
    }

    // Wrap the collapsed and expanded content in the reusable editor
    return (
      <div key={`fk_${fk.constraintName || i}`}>
        <ExpandableEditor
          editorExpanded={expandedContent}
          expandedLabel={expandedLabel}
          property={`fk-${i}`}
          service="modify-table"
          removeFunc={removeFk}
          saveFunc={saveFk}
          collapsedLabel={collapsedLabel}
          collapseCallback={resetFk}
          collapseButtonText={collapseButtonText}
          expandButtonText={expandButtonText}
          isToggled={isLast ? false : null}
        />
      </div>
    );
  });
};

export default ForeignKeyEditor;
