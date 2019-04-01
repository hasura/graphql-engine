import React from 'react';
import Select, { components } from 'react-select';

import dataTypes from '../Common/DataTypes';
import { getDataOptions } from './utils';

const CustomOption = props => {
  return (
    <div title={props.data.description}>
      <components.Option {...props} />
    </div>
  );
};

class TableColumn extends React.Component {
  render() {
    const {
      column,
      columns,
      colIndex: i,
      onRemoveColumn,
      onColumnChange,
      onColTypeChange,
      setColDefaultValue,
      onColNullableChange,
      onColUniqueChange,
      dataTypes: restTypes,
    } = this.props;
    const customStyles = {
      container: provided => ({
        ...provided,
        cursor: 'pointer',
      }),
      dropdownIndicator: provided => {
        return {
          ...provided,
          padding: '5px',
        };
      },
    };

    const styles = require('../../../Common/TableCommon/Table.scss');
    const handleColTypeChange = selectedOption => {
      onColTypeChange(
        selectedOption.colIdentifier,
        column.nullable,
        selectedOption.value
      );
    };
    let removeIcon;
    if (i + 1 === columns.length) {
      removeIcon = <i className={`${styles.fontAwosomeClose}`} />;
    } else {
      removeIcon = (
        <i
          className={`${styles.fontAwosomeClose} fa-lg fa fa-times`}
          onClick={onRemoveColumn.bind(this, i)}
        />
      );
    }
    let defValue = '';
    if ('default' in column) {
      defValue = column.default.value;
    }
    let defPlaceholder = 'default_value';
    if (column.type === 'timestamptz') {
      defPlaceholder = 'example: now()';
    } else if (column.type === 'date') {
      defPlaceholder = '';
    } else if (column.type === 'uuid') {
      defPlaceholder = 'example: gen_random_uuid()';
    }
    const { columnDataTypes, columnTypeValueMap } = getDataOptions(
      dataTypes,
      restTypes,
      i
    );
    return (
      <div key={i} className={`${styles.display_flex} form-group`}>
        <input
          type="text"
          className={`${styles.input} form-control ${styles.add_mar_right}`}
          value={column.name}
          placeholder="column_name"
          onChange={onColumnChange.bind(this, i, column.nullable || false)}
          data-test={`column-${i}`}
        />
        <Select
          isSearchable
          components={{ Option: CustomOption }}
          className={`${styles.select} ${styles.select200}`}
          classNamePrefix={`add_table_column_selector col-type-${i}`}
          placeholder="column_type"
          options={columnDataTypes}
          onChange={handleColTypeChange}
          value={column.type && columnTypeValueMap[column.type]}
          styles={customStyles}
        />
        <input
          placeholder={defPlaceholder}
          type="text"
          value={defValue}
          className={`
          ${styles.inputDefault} ${styles.defaultWidth} form-control ${
        styles.add_pad_left
      }`}
          onChange={setColDefaultValue.bind(this, i, column.nullable || false)}
          data-test={`col-default-${i}`}
        />{' '}
        <input
          className={`${styles.inputCheckbox} form-control `}
          checked={columns[i].nullable}
          type="checkbox"
          onChange={onColNullableChange.bind(this, i)}
          data-test={`nullable-${i}`}
        />{' '}
        <label>Nullable</label>
        <input
          className={`${styles.inputCheckbox} form-control `}
          checked={columns[i].unique || false}
          type="checkbox"
          onChange={onColUniqueChange.bind(this, i)}
          data-test={`unique-${i.toString()}`}
        />{' '}
        <label>Unique</label>
        {removeIcon}
      </div>
    );
  }
}

export default TableColumn;
