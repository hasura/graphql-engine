import React from 'react';
import { FaTimes } from 'react-icons/fa';
import { inputStyles } from '../../constants';

const UniqueKeySelector = ({
  uniqueKeys,
  uniqueKey,
  setUniqueKeys,
  numUniqueKeys,
  dispatch,
  index,
  columns,
  service,
}) => {
  const numCols = uniqueKey.length;

  // dispatch unique col select
  const dispatchSelectUniqueCol = (e, _i) => {
    const newUniqueKeys = JSON.parse(JSON.stringify(uniqueKeys));
    newUniqueKeys[index] = newUniqueKeys[index] || [];
    newUniqueKeys[index][_i] = parseInt(e.target.value, 10);
    if (newUniqueKeys[numUniqueKeys - 1].length && service === 'add-table') {
      newUniqueKeys.push([]);
    }
    dispatch(setUniqueKeys(newUniqueKeys));
  };

  // dispatch unique col remove
  const dispatchRemoveUniqueCol = (e, _i) => {
    const newUniqueKeys = JSON.parse(JSON.stringify(uniqueKeys));
    newUniqueKeys[index] = [
      ...newUniqueKeys[index].slice(0, _i),
      ...newUniqueKeys[index].slice(_i + 1),
    ];
    dispatch(setUniqueKeys(newUniqueKeys));
  };

  // select options
  const getColumnOptions = () => {
    return columns.map(c => {
      if (uniqueKey.includes(c.index) || !c.name || !c.type) {
        return null;
      }
      return (
        <option key={c.index} value={c.index}>
          {c.name}
        </option>
      );
    });
  };

  // selected columns
  const existingSelects = uniqueKey.map((uk, i) => {
    const removeUniqueCol = e => dispatchRemoveUniqueCol(e, i);
    const setUniqueCol = e => dispatchSelectUniqueCol(e, i);

    const removeIcon = (
      <FaTimes
        className="w-4 ml-sm cursor-pointer"
        data-test={`remove-uk-${index}-column-${i}`}
        onClick={removeUniqueCol}
      />
    );

    return (
      <div key={i} className="flex items-center">
        <select
          className="form-control"
          data-test={`unique-key-${index}-column-${i}`}
          value={uk}
          onChange={setUniqueCol}
        >
          <option key={uk} value={uk}>
            {columns[uk].name}
          </option>
          {getColumnOptions()}
        </select>
        {removeIcon}
      </div>
    );
  });

  // placeholder dropdown to add more columns
  const newSelect = () => {
    const selectUniqueCol = e => dispatchSelectUniqueCol(e, numCols);
    return (
      <div className="flex" key={numCols}>
        <select
          className={inputStyles}
          data-test={`unique-key-${index}-column-${numCols}`}
          onChange={selectUniqueCol}
          value={''}
        >
          <option key="uk-column-placeholder" value={''}>
            -- select --
          </option>
          {getColumnOptions()}
        </select>
        <div className="ml-sm w-4" />
      </div>
    );
  };

  return (
    <div>
      <span className="flex items-center text-gray-600 font-semibold mb-formlabel">
        Unique Keys:
      </span>
      <div className="space-y-md">
        {existingSelects}
        {newSelect()}
      </div>
    </div>
  );
};

export default UniqueKeySelector;
