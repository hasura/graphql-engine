import React from 'react';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import ForeignKeySelector from '../Common/ReusableComponents/ForeignKeySelector';
import { getForeignKeyConfig } from '../Common/ReusableComponents/utils';
import { setForeignKeys } from './AddActions';

const ForeignKeyWrapper = ({
  foreignKeys,
  allSchemas,
  columns,
  dispatch,
  styles,
}) => {
  const orderedColumns = columns
    .filter(c => Boolean(c.name))
    .map((c, i) => ({
      name: c.name,
      type: c.type,
      index: i,
    }));
  const refTables = {};
  allSchemas.forEach(tableSchema => {
    refTables[tableSchema.table_name] = tableSchema.columns.map(
      c => c.column_name
    );
  });

  // TODO check out match full
  return foreignKeys.map((fk, i) => {
    const fkConfig = getForeignKeyConfig(fk, orderedColumns);
    const collapsedLabel = () => (
      <div>
        <div className="container-fluid">
          <div className="row">
            <h5 className={styles.padd_bottom}>
              {fkConfig ? <i> {fkConfig} </i> : null}
              &nbsp;
            </h5>
          </div>
        </div>
      </div>
    );
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

    const expandButtonText = isLast ? 'Add a new foreign key' : 'Edit';

    return (
      <div key={`${i}_${isLast}`}>
        <ExpandableEditor
          editorExpanded={expandedContent}
          property={'add-fks'}
          ongoingRequest={'oola'}
          service="modify-table"
          removeButtonColor="white"
          removeFunc={removeFk}
          collapsedLabel={collapsedLabel}
          expandButtonText={expandButtonText}
          isCollapsable={false}
          toggled={!isLast}
        />
      </div>
    );
  });
};

export default ForeignKeyWrapper;
