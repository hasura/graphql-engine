import React from 'react';
import styles from '../../../../Common/TableCommon/Table.scss';

const PrimaryKeySelector = ({ primaryKeys, columns, setPk, dispatch }) => {
  const numPks = primaryKeys.length;
  const nonPkColumns = columns
    .map((col, _i) => {
      if (!primaryKeys.includes(_i.toString()) && col.name && col.type) {
        return {
          name: col.name,
          index: _i,
        };
      }
      return null;
    })
    .filter(rc => Boolean(rc));
  return primaryKeys.map((pk, i) => {
    let removeIcon;
    const dispatchSet = e => {
      const newPks = [
        ...primaryKeys.slice(0, i),
        e.target.value,
        ...primaryKeys.slice(i + 1),
      ];
      if (i + 1 === primaryKeys.length) {
        newPks.push('');
      }
      dispatch(setPk(newPks));
    };
    const dispatchRemove = () => {
      const newPks = [...primaryKeys.slice(0, i), ...primaryKeys.slice(i + 1)];
      dispatch(setPk(newPks));
    };
    if (i + 1 === numPks) {
      removeIcon = null;
    } else {
      removeIcon = (
        <i
          className={`${styles.fontAwosomeClose} fa-lg fa fa-times`}
          onClick={dispatchRemove}
        />
      );
    }

    return (
      <div key={i} className={`form-group ${styles.pkEditorWrapper}`}>
        <select
          value={pk || ''}
          className={`${styles.select} ${styles.sample} form-control ${
            styles.add_pad_left
          }`}
          onChange={dispatchSet}
          data-test={`primary-key-select-${i}`}
          data-test={`primary-key-select-${i.toString()}`}
        >
          {pk === '' ? (
            <option disabled value="">
              -- select --
            </option>
          ) : (
            <option value={pk}>{columns[pk].name}</option>
          )}
          {nonPkColumns.map(({ name, index }, j) => (
            <option key={j} value={index}>
              {name}
            </option>
          ))}
        </select>
        {removeIcon}
      </div>
    );
  });
};

export default PrimaryKeySelector;
