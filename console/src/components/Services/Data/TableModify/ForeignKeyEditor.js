import React from 'react';
import {
  setForeignKeys,
  saveForeignKeys,
  removeForeignKey,
} from './ModifyActions';
import { getForeignKeyConfig } from '../Common/Components/utils';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import ForeignKeySelector from '../Common/Components/ForeignKeySelector';

import { getConfirmation } from '../../../Common/utils/jsUtils';

const ForeignKeyEditor = ({
  tableSchema,
  allSchemas,
  dispatch,
  fkModify,
  schemaList,
  readOnlyMode,
  orderedColumns,
  existingForeignKeys,
}) => {
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

    const orderedSchemaList = schemaList.sort();

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
      const collapsedLabelText = getFkConfigLabel(fkConfig);
      if (!collapsedLabelText) return null;
      return <div className="text-gray-600">{collapsedLabelText}</div>;
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
      expandButtonText =
        numFks === 1 ? 'Add a foreign key' : 'Add a new foreign key';
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
        const isOk = getConfirmation();
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

    // Remove rows that have potentially been removed but has a residual state after race conditions
    if (!collapsedLabel() && !isLast) return null;
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
          readOnlyMode={readOnlyMode}
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
