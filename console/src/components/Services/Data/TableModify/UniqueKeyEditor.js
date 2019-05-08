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

  const initialiseState = () => {
    const existingConstraints = tableSchema.unique_constraints;
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
  }, []);

  const numUniqueKeys = uniqueKeys.length;

  return uniqueKeys.map((uniqueKey, i) => {
    const isLast = numUniqueKeys === i + 1;

    const uniqueKeyConfig = getUniqueKeyConfig(
      uniqueKey.map(uk => orderedColumns[uk].name)
    );

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

    const collapsedLabel = () => {
      if (!uniqueKeyConfig) return null;
      return (
        <div>
          <b>{uniqueKeyConfig}</b>
        </div>
      );
    };

    const expandedLabel = () => {
      if (!uniqueKeyConfig) return null;
      return (
        <div>
          <b>{uniqueKeyConfig}</b>
        </div>
      );
    };

    const saveFunc = toggle => {
      toggle();
      saveUniqueKey();
    };

    let removeFunc;
    if (!isLast) {
      removeFunc = toggle => {
        const isOk = window.confirm(
          'Are you sure you want to remove this unique key constraint?'
        );
        if (!isOk) {
          return;
        }
        toggle();
        dispatch(removeUniqueKey(i, tableSchema.table_name));
      };
    }

    let expandButtonText;
    if (!isLast) {
      expandButtonText = 'Edit';
    } else {
      expandButtonText =
        numUniqueKeys === 1 ? 'Add a unique key' : 'Add a new unique key';
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
          isCollapsable
        />
      </div>
    );
  });
};

export default UniqueKeyEditor;
