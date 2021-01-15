import React from 'react';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import ForeignKeySelector from '../Common/Components/ForeignKeySelector';
import { getForeignKeyConfig } from '../Common/Components/utils';
import { setForeignKeys, toggleFk, clearFkToggle } from './AddActions';

const ForeignKeyWrapper = ({
  foreignKeys,
  allSchemas,
  currentSchema,
  columns,
  dispatch,
  fkToggled,
  schemaList,
}) => {
  // columns in the right order with their indices
  const orderedColumns = columns
    .filter(c => Boolean(c.name))
    .map((c, i) => ({
      name: c.name,
      type: c.type,
      index: i,
    }));

  const numFks = foreignKeys.length;

  // TODO check out match full

  // Map the foreign keys in the fkModify state and render
  return foreignKeys.map((fk, i) => {
    const fkConfig = getForeignKeyConfig(fk, orderedColumns);
    const isLast = i + 1 === numFks;

    fk.refSchemaName = fk.refSchemaName || currentSchema;

    // Generate a list of reference tables and their columns
    const refTables = {};
    allSchemas.forEach(tableSchema => {
      if (fk.refSchemaName === tableSchema.table_schema) {
        refTables[tableSchema.table_name] = tableSchema.columns.map(
          c => c.column_name
        );
      }
    });

    const orderedSchemaList = schemaList.sort();

    // The content when the editor is expanded
    const expandedContent = () => (
      <ForeignKeySelector
        refTables={refTables}
        foreignKey={fk}
        index={i}
        service="add-table"
        foreignKeys={foreignKeys}
        orderedColumns={orderedColumns}
        dispatch={dispatch}
        setForeignKeys={setForeignKeys}
        schemaList={orderedSchemaList}
      />
    );
    // TODO handle ongoing request

    // Function to remove FK (is undefined for the last FK)
    let removeFk;
    if (!isLast) {
      removeFk = () => {
        const newFks = [
          ...foreignKeys.slice(0, i),
          ...foreignKeys.slice(i + 1),
        ];
        dispatch(setForeignKeys(newFks));
        dispatch(clearFkToggle());
      };
    }

    // Label to show next to the 'Edit' button (the FK configuration)
    let collapsedLabelText;
    if (fkConfig) {
      collapsedLabelText = <b>{fkConfig}</b>;
    } else if (isLast && numFks === 1) {
      collapsedLabelText = <i>(You can add foreign keys later as well)</i>;
    }

    const collapsedLabel = () => <div>{collapsedLabelText}</div>;

    const expandedLabel = () => {
      return (
        <div>
          <b>{fkConfig}</b>
        </div>
      );
    };

    // The collapse button text when the editor is collapsed
    let expandButtonText = isLast ? 'Add another foreign key' : 'Edit';
    if (numFks === 1) expandButtonText = 'Add a foreign key';

    let saveCallback;
    if (fkConfig) {
      saveCallback = () => {
        dispatch(clearFkToggle());
      };
    }

    const expandCallback = () => {
      dispatch(toggleFk(i));
    };

    const collapseCallback = fkConfig ? saveCallback : removeFk;

    // Wrap the collapsed and expanded content in the reusable editor
    return (
      <div key={`foreign-key-${i}`}>
        <ExpandableEditor
          editorExpanded={expandedContent}
          expandedLabel={expandedLabel}
          collapsedLabel={collapsedLabel}
          property={`fk-${i}`}
          service="add-table"
          removeFunc={removeFk}
          saveFunc={saveCallback}
          expandButtonText={expandButtonText}
          isCollapsable
          expandCallback={expandCallback}
          collapseCallback={collapseCallback}
          toggled={fkToggled === i}
        />
      </div>
    );
  });
};

export default ForeignKeyWrapper;
