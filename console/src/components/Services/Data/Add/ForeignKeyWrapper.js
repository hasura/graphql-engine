import React from 'react';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import ForeignKeySelector from '../Common/ReusableComponents/ForeignKeySelector';
import { getForeignKeyConfig } from '../Common/ReusableComponents/utils';
import { setForeignKeys, toggleFk, clearFkToggle } from './AddActions';

const ForeignKeyWrapper = ({
  foreignKeys,
  allSchemas,
  columns,
  dispatch,
  styles,
  fkToggled,
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
    const numFks = foreignKeys.length;
    const isLast = i + 1 === numFks;
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
    const fkConfig = getForeignKeyConfig(fk, orderedColumns);
    const collapsedLabelText =
      isLast && numFks === 1 ? (
        <i>(You can add foreign keys later as well)</i>
      ) : (
        <b>{fkConfig}</b>
      );
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

    // The collapse button text when the editor is collapsed
    let expandButtonText = isLast ? 'Add a new foreign key' : 'Edit';
    if (numFks === 1) expandButtonText = 'Add a foreign key';

    const collapseCallback = () => {
      dispatch(clearFkToggle());
    };

    const expandCallback = () => {
      dispatch(toggleFk(i));
    };

    // Wrap the collapsed and expanded content in the reusable editor
    return (
      <div key={`${i}`}>
        <ExpandableEditor
          editorExpanded={expandedContent}
          collapsedLabel={collapsedLabel}
          property={`fk-${i}`}
          service="add-table"
          removeFunc={removeFk}
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
