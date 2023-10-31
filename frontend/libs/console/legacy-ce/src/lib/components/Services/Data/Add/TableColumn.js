import { useEffect, useState } from 'react';
import PropTypes from 'prop-types';
import clsx from 'clsx';

import { getDataOptions, inferDefaultValues } from '../Common/utils';

import TableColumnDefault from './TableColumnDefault';
import { ColumnTypeSelector } from '../Common/Components/ColumnTypeSelector';
import { dataSource, isFeatureSupported } from '../../../../dataSources';
import { FaTimes } from 'react-icons/fa';
import { focusYellowRing, inputStyles } from '../utils';

const getNewType = ({ oldType = '', isArray }) =>
  isArray ? `${oldType.replace('[]', '')}[]` : oldType.replace('[]', '');

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

  const areArrayTypesSupported = isFeatureSupported('tables.create.arrayTypes');

  const [colTypeIdentifier, setColTypeIdentifier] = useState(i);

  const [colTypeValue, setColTypeValue] = useState(column.type || '');

  useEffect(() => {
    setColTypeValue(column.type || '');
  }, [column.type]);

  const [isArray, setArray] = useState(false);

  const handleColTypeChange = selectedOption => {
    if (!selectedOption) return onColTypeChange(i, '');

    const newType = getNewType({ oldType: selectedOption.value, isArray });

    onColTypeChange(selectedOption.colIdentifier, newType);

    setColTypeIdentifier(selectedOption.colIdentifier);
    setColTypeValue(newType);
  };

  const onColArrayChange = e => {
    const isArray = e.target.checked;

    setArray(isArray);
    if (colTypeValue) {
      const newType = getNewType({ oldType: colTypeValue, isArray });

      onColTypeChange(colTypeIdentifier, newType);
      setColTypeValue(newType);
    }
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

  const checkboxClassNames = clsx(focusYellowRing, 'cursor-pointer"');

  return (
    <div key={i} className="mb-sm gap-sm grid grid-flow-col auto-cols-max">
      <input
        type="text"
        className={`${inputStyles}`}
        value={column.name}
        placeholder="column_name"
        onChange={onColumnChange.bind(undefined, i, column.nullable || false)}
        data-test={`column-${i}`}
      />
      <span data-test={`col-type-${i}`} className="w-[160px]">
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

      {areArrayTypesSupported && (
        <label className="flex items-center mr-sm cursor-pointer">
          <input
            className={checkboxClassNames}
            style={{ margin: '0' }}
            checked={column.array}
            type="checkbox"
            onChange={onColArrayChange}
            data-test={`nullable-${i}`}
          />
          <span className="ml-xs">Array</span>
        </label>
      )}

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
        <label className="flex items-center mr-sm cursor-pointer">
          <input
            className={checkboxClassNames}
            style={{ margin: '0' }}
            checked={column.nullable}
            type="checkbox"
            onChange={onColNullableChange.bind(undefined, i)}
            data-test={`nullable-${i}`}
          />
          <span className="ml-xs">Nullable</span>
        </label>

        <label className="flex items-center cursor-pointer">
          <input
            className={checkboxClassNames}
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
        <div className="ml-2">{getRemoveIcon(colLength)}</div>
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
