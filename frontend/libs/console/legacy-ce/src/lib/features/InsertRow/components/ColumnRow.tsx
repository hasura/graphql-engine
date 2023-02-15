import clsx from 'clsx';
import { useEffect, useRef } from 'react';

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
  onChange: ({ columnName, selectionType, value }: RowValue) => void;
  resetToken: string;
};

export const ColumnRow = ({
  isDisabled,
  isNullDisabled,
  isDefaultDisabled,
  label,
  name,
  onChange,
  resetToken,
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

  const baseInputTw =
    'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400 placeholder-gray-500';
  const disabledInputTw = '!bg-slate-100 !border-slate-200 cursor-not-allowed';
  const inputTw = clsx(baseInputTw, isDisabled && disabledInputTw);

  const baseLabelTw = 'text-muted';
  const labelTw = clsx(baseLabelTw, 'w-48');
  const disabledLabelTw = 'text-slate-300 !cursor-not-allowed';
  const radioLabelTw = clsx(baseLabelTw, 'cursor-pointer');

  const radioTw =
    'cursor-pointer focus-within:outline-0 focus-within:ring-2 focus-within:ring-yellow-200 focus-within:border-yellow-400 !m-0';

  const disabledRadioTw = '!cursor-not-allowed border-slate-300';

  const onValueChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    onChange({
      columnName: name,
      selectionType: 'value',
      value: e.target.value,
    });
  };

  const onRadioChange = (e: any) => {
    const selectionType = e.target.value;

    if (
      valueInputRef?.current &&
      (selectionType === 'null' || selectionType === 'default')
    ) {
      valueInputRef.current.value = '';
    }

    onChange({
      columnName: name,
      selectionType: selectionType,
      value: undefined,
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
      <input
        name={name}
        onChange={onValueChange}
        onInput={checkValueRadio}
        ref={valueInputRef}
        type="text"
        className={inputTw}
        disabled={isDisabled}
        tabIndex={2}
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
    </div>
  );
};
