import React from 'react';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import UniqueKeySelector from '../Common/ReusableComponents/UniqueKeySelector';
import { getUniqueKeyConfig } from '../Common/ReusableComponents/utils';

const UniqueKeyWrapper = ({
  // allSchemas,
  columns,
  uniqueKeys,
  dispatch,
  setUniqueKeys,
}) => {
  const orderedColumns = columns.map(({ name, type }, i) => ({
    index: i,
    name,
    type,
  }));
  const numUniqueKeys = uniqueKeys.length;
  const uniqueKeyEditors = uniqueKeys.map((uniqueKey, i) => {
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
          service="add-table"
        />
      );
    };

    const collapsedLabel = () => {
      if (isLast && numUniqueKeys === 1) {
        return (
          <div>
            <i>(You can add unique keys later as well)</i>
          </div>
        );
      }
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

    const saveFunc = toggle => toggle();

    let removeFunc;
    if (!isLast) {
      removeFunc = toggle => {
        toggle();
        dispatch(
          setUniqueKeys([...uniqueKeys.slice(0, i), ...uniqueKeys.slice(i + 1)])
        );
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

  return <div>{uniqueKeyEditors}</div>;
};

export default UniqueKeyWrapper;
