import React, { useEffect } from 'react';
import { ordinalColSort } from '../utils';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import UniqueKeySelector from '../Common/ReusableComponents/UniqueKeySelector';
import {
  getUkeyPkeyConfig,
  getKeyDef,
} from '../Common/ReusableComponents/utils';
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

  const clearState = () => {
    dispatch(setUniqueKeys([[]]));
  };

  useEffect(() => {
    initialiseState();
    return clearState;
  }, [tableSchema]);

  // number of unique keys
  const numUniqueKeys = uniqueKeys.length;

  // iterate over the unique keys
  return uniqueKeys.map((uniqueKey, i) => {
    // Is this the last placeholder unique key
    const isLast = numUniqueKeys === i + 1;

    // get constraint name
    let constraintName = '';
    if (!isLast && existingConstraints[i]) {
      constraintName = existingConstraints[i].constraint_name;
    }

    // unique key config text
    const uniqueKeyConfig = existingConstraints[i]
      ? getUkeyPkeyConfig(existingConstraints[i].columns)
      : null;

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
      if (isLast) {
        if (numUniqueKeys === 1) {
          return 'No unique keys';
        }
        return null;
      }

      return getKeyDef(uniqueKeyConfig, constraintName);
    };

    // label text when unique key is expanded
    const expandedLabel = () => {
      return getKeyDef(uniqueKeyConfig, constraintName);
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
        saveFunc = () => {
          dispatch(
            saveUniqueKey(
              i,
              tableSchema.table_name,
              orderedColumns,
              existingConstraints
            )
          );
        };
      }
    } else {
      if (uniqueKey.length > 0) {
        saveFunc = () =>
          dispatch(
            saveUniqueKey(
              i,
              tableSchema.table_name,
              orderedColumns,
              existingConstraints
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
      expandButtonText = numUniqueKeys === 1 ? 'Add' : 'Add a new unique key';
      collapseButtonText = 'Cancel';
    }

    return (
      <div key={`unique-key-${constraintName || i}`}>
        <ExpandableEditor
          editorExpanded={expandedContent}
          expandedLabel={expandedLabel}
          collapsedLabel={collapsedLabel}
          property={`unique-key-${i}`}
          service="modify-table"
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
