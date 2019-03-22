import React from 'react';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import ForeignKeySelector from '../Common/ReusableComponents/ForeignKeySelector';
import { setForeignKeys } from './AddActions';

const ForeignKeyWrapper = ({
  foreignKeys,
  allSchemas,
  columns,
  dispatch,
  styles,
}) => {
  // columns in the right order with their indices
  const orderedColumns = columns
    .filter(c => Boolean(c.name))
    .map((c, i) => ({
      name: c.name,
      type: c.type,
      index: i,
    }));

  // Generate a list of reference tables and their columns
  const refTables = {};
  allSchemas.forEach(tableSchema => {
    refTables[tableSchema.table_name] = tableSchema.columns.map(
      c => c.column_name
    );
  });

  // TODO check out match full

  // Map the foreign keys in the fkModify state and render
  return foreignKeys.map((fk, i) => {
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
        styles={styles}
        setForeignKeys={setForeignKeys}
      />
    );

    // TODO handle ongoing request

    // Function to remove FK (is undefined for the last FK)
    let removeFk;
    const isLast = i + 1 === foreignKeys.length;
    if (!isLast) {
      removeFk = () => {
        const newFks = [
          ...foreignKeys.slice(0, i),
          ...foreignKeys.slice(i + 1),
        ];
        dispatch(setForeignKeys(newFks));
      };
    }

    // The collapse button text when the editor is collapsed
    const expandButtonText = isLast ? 'Add a new foreign key' : 'Edit';

    // Wrap the collapsed and expanded content in the reusable editor
    return (
      <div key={`${i}_${isLast}`}>
        <ExpandableEditor
          editorExpanded={expandedContent}
          property={'add-fks'}
          ongoingRequest={'oola'}
          service="modify-table"
          removeButtonColor="white"
          removeFunc={removeFk}
          expandButtonText={expandButtonText}
          isCollapsable={false}
          toggled={!isLast}
        />
      </div>
    );
  });
};

export default ForeignKeyWrapper;
