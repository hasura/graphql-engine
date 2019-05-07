import React from 'react';
// import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';

const getUniqueKeyConfig = columns => {
  return `( ${columns.join(', ')} )`;
};

const UniqueKeyWrapper = ({
  // allSchemas,
  columns,
  uniqueKeys,
}) => {
  const orderedColumns = columns.map((n, i) => ({
    index: i,
    name: n,
  }));

  return (
    <UniqueKeySelector
      func={getUniqueKeyConfig}
      cols={orderedColumns}
      keys={uniqueKeys}
    />
  );
};

const UniqueKeySelector = () => {
  return null;
};

export default UniqueKeyWrapper;
