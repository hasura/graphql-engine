import { dataSource } from '../../../dataSources';
import { TableColumn } from '../../DataSource';
import clsx from 'clsx';
import { useEffect, useRef } from 'react';
import { InputCustomEvent } from './TextInput';
import { ColumnRowInput } from './ColumnRowInput';
import { SupportedDrivers } from '../../hasura-metadata-types';
import { columnDataType } from '../../DataSource/utils';

type RowValue = {
  columnName: string;
  selectionType: 'value' | 'null' | 'default';
  value?: unknown;
};

export type ColumnRowProps = {
  isDisabled: boolean;
  isNullDisabled: boolean;
  isDefaultDisabled: boolean;
  label: string;
  name: string;
  dataType: TableColumn['dataType'];
  onChange: ({ columnName, selectionType, value }: RowValue) => void;
  resetToken: string;
  placeholder: string;
  driver: SupportedDrivers;
};

export const ColumnRow = ({
  isDisabled,
  isNullDisabled,
  isDefaultDisabled,
  label,
  name,
  onChange,
  resetToken,
  placeholder,
  dataType,
  driver,
}: ColumnRowProps) => {
  const valueId = `${name}-value`;
  const nullId = `${name}-null`;
  const defaultId = `${name}-default`;

  const valueRadioRef = useRef<HTMLInputElement>(null);
  const valueInputRef = useRef<HTMLInputElement>(null);

  const checkValueRadio = () => {
    if (valueRadioRef.current) {
      valueRadioRef.current.checked = true;
    }
  };

  const onCheckValueRadio = () => {
    checkValueRadio();
  };

  useEffect(() => {
    if (valueInputRef.current && !!valueInputRef.current.value) {
      valueInputRef.current.value = '';
      onChange({
        columnName: name,
        selectionType: 'default',
        value: '',
      });
    }

    if (valueRadioRef.current?.checked) {
      valueRadioRef.current.checked = false;
    }
  }, [resetToken]);

  const baseLabelTw = 'text-muted';
  const labelTw = clsx(baseLabelTw, 'min-w-[120px] w-48 font-semibold');
  const disabledLabelTw = 'text-slate-300 !cursor-not-allowed';
  const radioLabelTw = clsx(baseLabelTw, 'cursor-pointer');

  const radioTw =
    'cursor-pointer focus-within:outline-0 focus-within:ring-2 focus-within:ring-yellow-200 focus-within:border-yellow-400 !m-0';

  const disabledRadioTw = '!cursor-not-allowed border-slate-300';

  const onValueChange = (
    e: React.ChangeEvent<HTMLInputElement> | InputCustomEvent
  ) => {
    onChange({
      columnName: name,
      selectionType: 'value',
      value: e.target.value,
    });
  };

  const onRadioChange = (e: InputCustomEvent) => {
    const selectionType = e.target.value;

    if (
      valueInputRef &&
      valueInputRef?.current &&
      (selectionType === 'null' || selectionType === 'default')
    ) {
      valueInputRef.current.value = '';
    }

    const value =
      dataType === dataSource.columnDataTypes.BOOLEAN ? false : undefined;

    onChange({
      columnName: name,
      selectionType: selectionType as RowValue['selectionType'],
      value,
    });
  };

  const _isNullDisabled = isDisabled || isNullDisabled;
  const _isDefaultDisabled = isDisabled || isDefaultDisabled;

  return (
    <div className="flex flex-row items-center gap-3">
      <label htmlFor={valueId} className={labelTw}>
        {label}
      </label>
      <input
        className={clsx(radioTw, isDisabled && disabledRadioTw)}
        id={valueId}
        name={name}
        ref={valueRadioRef}
        type="radio"
        value="value"
        onChange={onRadioChange}
        disabled={isDisabled}
        tabIndex={1}
      />

      <ColumnRowInput
        dataType={columnDataType(dataType)}
        name={name}
        onChange={onValueChange}
        onInput={checkValueRadio}
        inputRef={valueInputRef}
        disabled={isDisabled}
        placeholder={placeholder}
        onValueChange={onValueChange}
        onCheckValueRadio={onCheckValueRadio}
        driver={driver}
      />

      <input
        className={clsx(radioTw, _isNullDisabled && disabledRadioTw)}
        id={nullId}
        name={name}
        type="radio"
        value="null"
        onChange={onRadioChange}
        disabled={_isNullDisabled}
        tabIndex={3}
      />
      <label
        htmlFor={nullId}
        className={clsx(radioLabelTw, _isNullDisabled && disabledLabelTw)}
      >
        NULL
      </label>
      <input
        className={clsx(radioTw, _isDefaultDisabled && disabledRadioTw)}
        id={defaultId}
        name={name}
        type="radio"
        value="default"
        onChange={onRadioChange}
        disabled={_isDefaultDisabled}
        tabIndex={4}
      />
      <label
        htmlFor={defaultId}
        className={clsx(radioLabelTw, _isDefaultDisabled && disabledLabelTw)}
      >
        Default
      </label>
      <span className="min-w-[180px] text-slate-400 font-mono text-sm font-light">
        ({dataType})
      </span>
    </div>
  );
};
