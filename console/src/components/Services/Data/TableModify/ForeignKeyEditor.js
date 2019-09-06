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
import { updateSchemaInfo } from '../DataActions';

const ForeignKeyEditor = ({
  tableSchema,
  allSchemas,
  dispatch,
  fkModify,
  schemaList,
}) => {
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
  const schemasToBeFetched = {};
  existingForeignKeys.forEach(efk => {
    schemasToBeFetched[efk.refSchemaName] = true;
  });
  existingForeignKeys.push({
    refSchemaName: '',
    refTableName: '',
    onUpdate: 'restrict',
    onDelete: 'restrict',
    colMappings: [{ column: '', refColumn: '' }],
  });
  useEffect(() => {
    dispatch(setForeignKeys(existingForeignKeys));
    dispatch(updateSchemaInfo({ schemas: Object.keys(schemasToBeFetched) }));
  }, []);

  const numFks = fkModify.length;

  // Map the foreign keys in the fkModify state and render
  return fkModify.map((fk, i) => {
    // FK config (example: (a, b) -> refTable(c, d))
    const fkConfig = getForeignKeyConfig(fk, orderedColumns);

    fk.refSchemaName = fk.refSchemaName || tableSchema.table_schema;

    // Generate a list of reference schemas and their columns
    const refTables = {};
    allSchemas.forEach(ts => {
      if (ts.table_schema === fk.refSchemaName) {
        refTables[ts.table_name] = ts.columns.map(c => c.column_name);
      }
    });

    const orderedSchemaList = schemaList.map(s => s.schema_name).sort();

    const getFkConfigLabel = config => {
      let fkConfigLabel;
      if (config) {
        fkConfigLabel = (
          <span>
            <b>{config}</b> - <i>{fk.constraintName}</i>
          </span>
        );
      }
      return fkConfigLabel;
    };

    const isLast = i + 1 === numFks;

    // Label to show next to the 'Edit' button (the FK configuration)
    const collapsedLabel = () => {
      const collapsedLabelText =
        isLast && numFks === 1 ? 'No foreign keys' : getFkConfigLabel(fkConfig);

      return <div>{collapsedLabelText}</div>;
    };

    // The content when the editor is expanded
    const expandedContent = () => (
      <ForeignKeySelector
        schemaList={orderedSchemaList}
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
      if (isLast) {
        return null;
      }

      const existingFkConfig = getForeignKeyConfig(
        existingForeignKeys[i],
        orderedColumns
      );

      return <div>{getFkConfigLabel(existingFkConfig)}</div>;
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
