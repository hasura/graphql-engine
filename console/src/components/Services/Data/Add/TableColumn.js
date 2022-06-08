import React from 'react';
import PropTypes from 'prop-types';

import { getDataOptions, inferDefaultValues } from '../Common/utils';

import TableColumnDefault from './TableColumnDefault';
import { ColumnTypeSelector } from '../Common/Components/ColumnTypeSelector';
import { dataSource, isFeatureSupported } from '../../../../dataSources';
import { FaTimes } from 'react-icons/fa';
import { focusYellowRing, inputStyles } from '../utils';

const TableColumn = props => {
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
      removeIcon = <FaTimes className="w-4 cursor-pointer" />;
    } else {
      removeIcon = (
        <FaTimes
          className="w-4 cursor-pointer"
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
    <div key={i} className="grid mb-sm gap-sm grid-cols-1 sm:grid-cols-5">
      <input
        type="text"
        className={`${inputStyles}`}
        value={column.name}
        placeholder="column_name"
        onChange={onColumnChange.bind(undefined, i, column.nullable || false)}
        data-test={`column-${i}`}
      />
      <span
        // className={`mt-sm w-72`}
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
            styleOverrides={focusYellowRing}
          />
        ) : (
          <input
            type="text"
            className={`${inputStyles} max-w-48 col-type-${i}`}
            onChange={e => {
              e.persist();
              onColTypeChange(i, e.target.value);
            }}
            placeholder="column_type"
          />
        )}
      </span>
      <span className={inputStyles}>
        <TableColumnDefault
          onChange={setColDefaultValue}
          colIndex={i}
          testId={`col-default-${i}`}
          column={column}
          colDefaultFunctions={defaultFunctions}
        />
      </span>
      <div className="flex items-center">
        <label className="flex items-center mr-sm">
          <input
            className={focusYellowRing}
            style={{ margin: '0' }}
            checked={column.nullable}
            type="checkbox"
            onChange={onColNullableChange.bind(undefined, i)}
            data-test={`nullable-${i}`}
          />
          <span className="ml-xs">Nullable</span>
        </label>
        <label className="flex items-center">
          <input
            className={focusYellowRing}
            style={{ margin: '0' }}
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
          <span className="ml-xs">Unique</span>
        </label>
        <div className="ml-auto">{getRemoveIcon(colLength)}</div>
      </div>
    </div>
  );
};

TableColumn.propTypes = {
  colIndex: PropTypes.number.isRequired,
  column: PropTypes.object.isRequired,
  colLength: PropTypes.number.isRequired,
};

export default TableColumn;
