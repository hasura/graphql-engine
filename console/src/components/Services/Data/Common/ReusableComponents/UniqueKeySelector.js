import React from 'react';
import styles from '../../../../Common/TableCommon/Table.scss';

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
      <i
        className={`${styles.fontAwosomeClose} fa-lg fa fa-times`}
        data-test={`remove-uk-${index}-column-${i}`}
        onClick={removeUniqueCol}
      />
    );

    return (
      <div key={i} className={`form-group ${styles.pkEditorWrapper}`}>
        <select
          className={`${styles.select} ${styles.sample} form-control ${
            styles.add_pad_left
          }`}
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
      <div key={numCols} className={`form-group ${styles.pkEditorWrapper}`}>
        <select
          className={`${styles.select} ${styles.sample} form-control ${
            styles.add_pad_left
          }`}
          data-test={`unique-key-${index}-column-${numCols}`}
          onChange={selectUniqueCol}
          value={''}
        >
          <option key="uk-column-placeholder" value={''}>
            -- select --
          </option>
          {getColumnOptions()}
        </select>
      </div>
    );
  };

  return (
    <div>
      {existingSelects}
      {newSelect()}
    </div>
  );
};

export default UniqueKeySelector;
