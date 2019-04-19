import React from 'react';
import PropTypes from 'prop-types';

import SearchableSelectBox from '../../../Common/SearchableSelect/SearchableSelect';
import dataTypes from '../Common/DataTypes';
import { getDataOptions, getPlaceholder, getDefaultValue } from './utils';

/* Custom style object for searchable select box */
const customStyles = {
  container: provided => ({
    ...provided,
  }),
  dropdownIndicator: provided => {
    return {
      ...provided,
      padding: '5px',
    };
  },
};

const TableColumn = props => {
  const styles = require('../../../Common/TableCommon/Table.scss');
  const {
    column,
    colLength,
    colIndex: i,
    onRemoveColumn,
    onColumnChange,
    onColTypeChange,
    setColDefaultValue,
    onColNullableChange,
    onColUniqueChange,
    dataTypes: restTypes,
  } = props;

  const handleColTypeChange = selectedOption => {
    onColTypeChange(selectedOption.colIdentifier, selectedOption.value);
  };
  const { columnDataTypes, columnTypeValueMap } = getDataOptions(
    dataTypes,
    restTypes,
    i
  );
  const getRemoveIcon = colLen => {
    let removeIcon;
    if (i + 1 === colLen) {
      removeIcon = <i className={`${styles.fontAwosomeClose}`} />;
    } else {
      removeIcon = (
        <i
          className={`${styles.fontAwosomeClose} fa-lg fa fa-times`}
          onClick={onRemoveColumn.bind(undefined, i)}
        />
      );
    }
    return removeIcon;
  };

  return (
    <div key={i} className={`${styles.display_flex} form-group`}>
      <input
        type="text"
        className={`${styles.input} form-control ${styles.add_mar_right}`}
        value={column.name}
        placeholder="column_name"
        onChange={onColumnChange.bind(undefined, i, column.nullable || false)}
        data-test={`column-${i}`}
      />
      <span
        className={`${styles.select} ${styles.select200}`}
        data-test={`col-type-${i}`}
      >
        <SearchableSelectBox
          options={columnDataTypes}
          onChange={handleColTypeChange}
          column={column.type && columnTypeValueMap[column.type]}
          bsClass={`col-type-${i} add_table_column_selector`}
          customStyle={customStyles}
        />
      </span>
      <input
        placeholder={getPlaceholder(column)}
        type="text"
        value={getDefaultValue(column)}
        className={`${styles.inputDefault} ${
          styles.defaultWidth
        } form-control ${styles.add_pad_left}`}
        onChange={setColDefaultValue.bind(
          undefined,
          i,
          column.nullable || false
        )}
        data-test={`col-default-${i}`}
      />{' '}
      <input
        className={`${styles.inputCheckbox} form-control `}
        checked={column.nullable}
        type="checkbox"
        onChange={onColNullableChange.bind(undefined, i)}
        data-test={`nullable-${i}`}
      />{' '}
      <label>Nullable</label>
      <input
        className={`${styles.inputCheckbox} form-control `}
        checked={column.unique || false}
        type="checkbox"
        onChange={onColUniqueChange.bind(undefined, i)}
        data-test={`unique-${i.toString()}`}
      />{' '}
      <label>Unique</label>
      {getRemoveIcon(colLength)}
    </div>
  );
};

TableColumn.propTypes = {
  colIndex: PropTypes.number.isRequired,
  column: PropTypes.object.isRequired,
  colLength: PropTypes.number.isRequired,
};

export default TableColumn;
