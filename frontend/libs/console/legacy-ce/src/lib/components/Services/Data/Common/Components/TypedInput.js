import React from 'react';

import { getPlaceholder } from '../../utils';
import JsonInput from '../../../../Common/CustomInputTypes/JsonInput';
import TextInput from '../../../../Common/CustomInputTypes/TextInput';
import styles from '../../../../Common/TableCommon/Table.module.scss';
import { dataSource } from '../../../../../dataSources';
import { onClick } from './typedInputUtils/onClick';

export const TypedInput = ({
  enumOptions,
  col,
  index,
  onChange,
  prevValue = null,
  hasDefault = false,
  disabled,
  values,
}) => {
  const {
    column_name: colName,
    data_type: colType,
    column_default: colDefault,
  } = col;

  const placeHolder = hasDefault ? colDefault : getPlaceholder(colType);
  const getDefaultValue = () => {
    if (
      values?.[colName] !== null &&
      values?.[colName] !== undefined &&
      !disabled
    ) {
      return values?.[colName];
    }
    return '';
  };

  const defaultValue = getDefaultValue();

  const standardInputProps = {
    onChange,
    onClick,
    disabled,
    'data-test': `typed-input-${index}`,
    className: `form-control ${styles.insertBox}`,
    defaultValue,
    type: 'text',
    placeholder: 'text',
  };

  if (disabled) {
    return <input {...standardInputProps} readOnly placeholder={placeHolder} />;
  }

  if (enumOptions && enumOptions[colName]) {
    return (
      <select
        {...standardInputProps}
        className={`form-control ${styles.insertBox}`}
        defaultValue={getDefaultValue()}
      >
        <option disabled value="">
          -- enum value --
        </option>
        {enumOptions[colName].map(option => (
          <option key={option} value={option}>
            {option}
          </option>
        ))}
      </select>
    );
  }

  if (prevValue && typeof prevValue === 'object') {
    return (
      <JsonInput
        standardProps={{
          ...standardInputProps,
          defaultValue: JSON.stringify(prevValue),
        }}
        placeholderProp={getPlaceholder(colType)}
      />
    );
  }

  switch (colType) {
    case dataSource.columnDataTypes.JSONB:
    case dataSource.columnDataTypes.JSONDTYPE:
      return (
        <JsonInput
          standardProps={{
            ...standardInputProps,
            defaultValue: prevValue
              ? JSON.stringify(prevValue)
              : getDefaultValue(),
          }}
          placeholderProp={placeHolder}
        />
      );

    case dataSource.columnDataTypes.TEXT:
      return (
        <TextInput
          standardProps={standardInputProps}
          placeholderProp={placeHolder}
        />
      );

    case dataSource.columnDataTypes.BOOLEAN:
      return (
        <select {...standardInputProps}>
          <option value="" disabled>
            -- bool --
          </option>
          <option value="true">True</option>
          <option value="false">False</option>
        </select>
      );

    case dataSource.columnDataTypes.ARRAY: {
      let defaultValue = standardInputProps.defaultValue;
      try {
        defaultValue =
          standardInputProps.defaultValue !== ''
            ? JSON.stringify(standardInputProps.defaultValue)
            : '';
      } catch (err) {
        console.error(err);
      }
      return (
        <input
          {...standardInputProps}
          defaultValue={defaultValue}
          placeholder={placeHolder}
        />
      );
    }

    default:
      return <input {...standardInputProps} placeholder={placeHolder} />;
  }
};
