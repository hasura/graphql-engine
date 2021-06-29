import React from 'react';

import { getPlaceholder } from '../../utils';
import JsonInput from '../../../../Common/CustomInputTypes/JsonInput';
import TextInput from '../../../../Common/CustomInputTypes/TextInput';
import styles from '../../../../Common/TableCommon/Table.scss';
import { dataSource } from '../../../../../dataSources';

export const TypedInput = ({
  enumOptions,
  col,
  index,
  clone,
  inputRef,
  onChange,
  onFocus,
  prevValue = null,
  hasDefault = false,
  disabled,
}) => {
  const {
    column_name: colName,
    data_type: colType,
    column_default: colDefault,
  } = col;

  const placeHolder = hasDefault ? colDefault : getPlaceholder(colType);
  const getDefaultValue = () => {
    if (clone && colName in clone) return clone[colName];
    if (prevValue !== undefined) {
      return prevValue === null ? '' : prevValue;
    }
    return '';
  };

  const onClick = e => {
    e.target
      .closest('.radio-inline')
      .querySelector('input[type="radio"]').checked = true;
    e.target.focus();
  };

  const standardInputProps = {
    onChange,
    onFocus,
    onClick,
    disabled,
    ref: inputRef,
    'data-test': `typed-input-${index}`,
    className: `form-control ${styles.insertBox}`,
    defaultValue: getDefaultValue(),
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
        defaultValue={prevValue || ''}
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

    default:
      return <input {...standardInputProps} placeholder={placeHolder} />;
  }
};
