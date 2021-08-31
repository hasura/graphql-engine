import React from 'react';
import PropTypes from 'prop-types';

import { getDataOptions, inferDefaultValues } from '../Common/utils';

import TableColumnDefault from './TableColumnDefault';
import { ColumnTypeSelector } from '../Common/Components/ColumnTypeSelector';
import { dataSource, isFeatureSupported } from '../../../../dataSources';

/* Custom style object for searchable select box */
const customSelectBoxStyles = {
  dropdownIndicator: {
    padding: '5px',
  },
  singleValue: {
    color: '#555555',
  },
  valueContainer: {
    padding: '0px 12px',
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
    columnDefaultFunctions,
    columnTypeCasts,
    uniqueKeys,
  } = props;

  let isColumnUnique = false;
  let _uindex;
  const numUniqueKeys = uniqueKeys.length;
  for (let _i = numUniqueKeys - 1; _i >= 0; _i--) {
    const key = uniqueKeys[_i];
    if (key.length === 1) {
      if (key[0] === i) {
        isColumnUnique = true;
        _uindex = _i;
      }
    }
  }

  const handleColTypeChange = selectedOption => {
    onColTypeChange(selectedOption.colIdentifier, selectedOption.value);
  };
  const { columnDataTypes, columnTypeValueMap } = getDataOptions(
    dataSource.commonDataTypes,
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

  /* Collect list of relevant default values if the type doesn't have any default values
   * */
  const getInferredDefaultValues = () =>
    inferDefaultValues(columnDefaultFunctions, columnTypeCasts)(column.type);

  const defaultFunctions =
    column.type in columnDefaultFunctions
      ? columnDefaultFunctions[column.type]
      : getInferredDefaultValues();

  return (
    <div key={i} className={`${styles.display_flex} form-group`}>
      <input
        type="text"
        className={`${styles.input} form-control`}
        value={column.name}
        placeholder="column_name"
        onChange={onColumnChange.bind(undefined, i, column.nullable || false)}
        data-test={`column-${i}`}
      />
      <span
        className={`${styles.inputDefault} ${styles.defaultWidth}`}
        data-test={`col-type-${i}`}
      >
        {isFeatureSupported('tables.create.frequentlyUsedColumns') ? (
          <ColumnTypeSelector
            options={columnDataTypes}
            onChange={handleColTypeChange}
            value={
              (column.type && columnTypeValueMap[column.type]) || column.type
            }
            colIdentifier={i}
            bsClass={`col-type-${i} add_table_column_selector`}
            styleOverrides={customSelectBoxStyles}
          />
        ) : (
          <input
            type="text"
            style={{ maxWidth: '200px' }}
            className={`${styles.input} form-control col-type-${i}`}
            onChange={e => {
              e.persist();
              onColTypeChange(i, e.target.value);
            }}
            placeholder="column_type"
          />
        )}
      </span>
      <span className={`${styles.inputDefault} ${styles.defaultWidth}`}>
        <TableColumnDefault
          onChange={setColDefaultValue}
          colIndex={i}
          testId={`col-default-${i}`}
          column={column}
          colDefaultFunctions={defaultFunctions}
        />
      </span>
      <label className="flex items-center">
        <input
          className={`${styles.inputCheckbox} legacy-input-fix`}
          checked={column.nullable}
          type="checkbox"
          onChange={onColNullableChange.bind(undefined, i)}
          data-test={`nullable-${i}`}
        />
        Nullable
      </label>
      <label className="flex items-center">
        <input
          className={`${styles.inputCheckbox} legacy-input-fix`}
          checked={isColumnUnique}
          type="checkbox"
          onChange={onColUniqueChange.bind(
            undefined,
            i,
            numUniqueKeys,
            isColumnUnique,
            _uindex
          )}
          data-test={`unique-${i.toString()}`}
        />
        Unique
      </label>
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
