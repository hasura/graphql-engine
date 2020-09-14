import React, { ComponentProps } from 'react';

import { JSONB, JSONDTYPE, TEXT, BOOLEAN, getPlaceholder } from '../../utils';
import JsonInput from '../../../../Common/CustomInputTypes/JsonInput';
import TextInput from '../../../../Common/CustomInputTypes/TextInput';
import styles from '../../../../Common/TableCommon/Table.scss';
import {
  isColumnAutoIncrement,
  TableColumn,
} from '../../../../Common/utils/pgUtils';
import SearchableSelect from '../../../../Common/SearchableSelect/SearchableSelect';
import { ForeignKeyValueSelect, FkColOption } from './ForeignKeyValueSelect';

type Option = { label: string; value: string };

type TableType = {
  name: string;
  type: string;
};
export type ReftablesType = {
  [id: string]: TableType[];
};

type Props = {
  enumOptions: Record<string, string[]>;
  col: TableColumn;
  index: number;
  clone: Record<string, any>;
  inputRef: React.Ref<any>;
  onChange: () => void;
  onFocus: () => void;
  prevValue: string;
  hasDefault: boolean;
  foreignKey: any;
  fkOptions: Array<FkColOption>;
  getFkOptions: (opts: FkColOption, value: string) => Promise<void>;
  refTables: ReftablesType; // TODO clean before commit
  onFkValueChange: ComponentProps<typeof SearchableSelect>['onChange'];
  selectedOption: Option;
};
const wrapperClassName = `${styles.radioLabel} ${styles.typedInputWrapper} radio-inline`;

export const TypedInput: React.FC<Props> = ({
  enumOptions,
  col,
  index,
  clone,
  inputRef,
  onChange,
  onFocus,
  prevValue,
  hasDefault,
  fkOptions,
  getFkOptions,
  refTables,
  foreignKey = false,
  onFkValueChange,
  selectedOption,
  children,
}) => {
  const {
    column_name: colName,
    data_type: colType,
    column_default: colDefault,
  } = col;
  const isAutoIncrement = isColumnAutoIncrement(col);
  const placeHolder = hasDefault ? colDefault : getPlaceholder(colType);
  const getDefaultValue = () => {
    if (prevValue !== undefined) return prevValue;
    if (clone && colName in clone) return clone[colName];
    return '';
  };

  const onClick = (
    e: React.MouseEvent<HTMLInputElement | HTMLSelectElement>
  ) => {
    const element = (e.target as HTMLInputElement)
      .closest('.radio-inline')
      ?.querySelector('input[type="radio"]');

    if (element) {
      (element as HTMLInputElement).checked = true;
    }

    (e.target as HTMLInputElement).focus();
  };

  const standardInputProps = {
    onChange,
    onFocus,
    onClick,
    ref: inputRef,
    'data-test': `typed-input-${index}`,
    className: `form-control ${styles.insertBox}`,
    defaultValue: getDefaultValue(),
    type: 'text',
    placeholder: 'text',
  };

  if (enumOptions && enumOptions[colName]) {
    return (
      <>
        <span className={wrapperClassName}>
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
        </span>
        {children}
      </>
    );
  }

  if (foreignKey) {
    delete standardInputProps.ref;
    return (
      <ForeignKeyValueSelect
        prevValue={prevValue}
        fkOptions={fkOptions}
        getFkOptions={getFkOptions}
        selectedOption={selectedOption}
        standardInputProps={standardInputProps}
        columnName={colName}
        refTables={refTables}
        foreignKey={foreignKey}
        placeholder={placeHolder}
        onFkValueChange={onFkValueChange}
        wrapperClassName={wrapperClassName}
      >
        {children}
      </ForeignKeyValueSelect>
    );
  }

  if (isAutoIncrement) {
    return (
      <>
        <span className={wrapperClassName}>
          <input {...standardInputProps} readOnly placeholder={placeHolder} />
        </span>
        {children}
      </>
    );
  }

  if (prevValue && typeof prevValue === 'object') {
    return (
      <>
        <span className={wrapperClassName}>
          <JsonInput
            standardProps={{
              ...standardInputProps,
              defaultValue: JSON.stringify(prevValue),
            }}
            placeholderProp={getPlaceholder(colType)}
          />
        </span>
        {children}
      </>
    );
  }

  switch (colType) {
    case JSONB:
    case JSONDTYPE:
      return (
        <>
          <span className={wrapperClassName}>
            <JsonInput
              standardProps={{
                ...standardInputProps,
                defaultValue: prevValue
                  ? JSON.stringify(prevValue)
                  : getDefaultValue(),
              }}
              placeholderProp={placeHolder}
            />
          </span>
          {children}
        </>
      );

    case TEXT:
      return (
        <>
          <span className={wrapperClassName}>
            <TextInput
              standardProps={standardInputProps}
              placeholderProp={placeHolder}
            />
          </span>
          {children}
        </>
      );

    case BOOLEAN:
      return (
        <>
          <span className={wrapperClassName}>
            <select {...standardInputProps}>
              <option value="" disabled>
                -- bool --
              </option>
              <option value="true">True</option>
              <option value="false">False</option>
            </select>
          </span>
          {children}
        </>
      );

    default:
      return (
        <>
          <span className={wrapperClassName}>
            <input {...standardInputProps} placeholder={placeHolder} />
          </span>
          {children}
        </>
      );
  }
};
