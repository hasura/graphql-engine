import React, { useEffect } from 'react';
import { ordinalColSort } from '../utils';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import UniqueKeySelector from '../Common/ReusableComponents/UniqueKeySelector';
import { getUniqueKeyConfig } from '../Common/ReusableComponents/utils';
import { saveUniqueKey, removeUniqueKey } from './ModifyActions';

const UniqueKeyEditor = ({
  uniqueKeys,
  tableSchema,
  setUniqueKeys,
  dispatch,
}) => {
  const columns = tableSchema.columns.sort(ordinalColSort);
  // columns in the right order with their indices
  const orderedColumns = columns.map((c, i) => ({
    name: c.column_name,
    index: i,
    type: c.data_type,
  }));

  // initialise unique key modify state with the existing unique constraints
  const existingConstraints = tableSchema.unique_constraints;
  const initialiseState = () => {
    dispatch(
      setUniqueKeys([
        ...existingConstraints.map(ec => {
          const cols = [];
          ec.columns.forEach(c => {
            cols.push(orderedColumns.find(oc => oc.name === c).index);
          });
          return cols;
        }),
        [],
      ])
    );
  };
  useEffect(() => {
    initialiseState();
  }, [tableSchema]);

  // number of unique keys
  const numUniqueKeys = uniqueKeys.length;

  // iterate over the unique keys
  return uniqueKeys.map((uniqueKey, i) => {
    // Is this the last placeholder unique key
    const isLast = numUniqueKeys === i + 1;

    // unique key config text
    const uniqueKeyConfig = getUniqueKeyConfig(
      uniqueKey.map(uk => orderedColumns[uk].name)
    );

    // content of the unique key editor
    const expandedContent = () => {
      return (
        <UniqueKeySelector
          dispatch={dispatch}
          uniqueKeys={uniqueKeys}
          setUniqueKeys={setUniqueKeys}
          uniqueKey={uniqueKey}
          index={i}
          numUniqueKeys={numUniqueKeys}
          columns={orderedColumns}
        />
      );
    };

    // label text when unique key is collapsed
    const collapsedLabel = () => {
      if (!uniqueKeyConfig) return null;
      return (
        <div>
          <b>{uniqueKeyConfig}</b>
        </div>
      );
    };

    // label text when unique key is expanded
    const expandedLabel = () => {
      if (!uniqueKeyConfig) return null;
      return (
        <div>
          <b>{uniqueKeyConfig}</b>
        </div>
      );
    };

    // remove unique key function (disabled if it is not an existing constraint)
    let removeFunc;
    if (!isLast) {
      removeFunc = toggle => {
        const isOk = window.confirm(
          'Are you sure you want to remove this unique key constraint?'
        );
        if (!isOk) {
          return;
        }
        dispatch(
          removeUniqueKey(
            i,
            tableSchema.table_name,
            existingConstraints,
            toggle
          )
        );
      };
    }

    // save unique key function
    let saveFunc;
    if (isLast) {
      if (uniqueKey.length > 0) {
        saveFunc = toggle => {
          dispatch(
            saveUniqueKey(
              i,
              tableSchema.table_name,
              orderedColumns,
              existingConstraints,
              toggle
            )
          );
        };
      }
    } else {
      if (uniqueKey.length > 0) {
        saveFunc = toggle =>
          dispatch(
            saveUniqueKey(
              i,
              tableSchema.table_name,
              orderedColumns,
              existingConstraints,
              toggle
            )
          );
      } else {
        saveFunc = removeFunc;
      }
    }

    // toggle button text of the expandable editor
    let expandButtonText;
    let collapseButtonText;
    if (!isLast) {
      expandButtonText = 'Edit';
      collapseButtonText = 'Close';
    } else {
      expandButtonText =
        numUniqueKeys === 1 ? 'Add a unique key' : 'Add a new unique key';
      collapseButtonText = 'Cancel';
    }

    return (
      <div key={i}>
        <ExpandableEditor
          editorExpanded={expandedContent}
          expandedLabel={expandedLabel}
          collapsedLabel={collapsedLabel}
          property={`unique-key-${i}`}
          service="add-table"
          saveFunc={saveFunc}
          removeFunc={removeFunc}
          expandButtonText={expandButtonText}
          collapseButtonText={collapseButtonText}
          isCollapsable
        />
      </div>
    );
  });
};

export default UniqueKeyEditor;
