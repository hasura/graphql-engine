import React, { ReactText } from 'react';
import clsx from 'clsx';

export type SelectItem = {
  label: ReactText;
  value: ReactText;
  disabled?: boolean;
};

export type SelectInputSplitFieldProps = {
  inputType?: 'text' | 'email' | 'password';
  selectOptions: SelectItem[];
  size?: 'full' | 'medium';
  placeholder?: string;
  inputDisabled?: boolean;
  selectDisabled?: boolean;
  inputValue: string;
  selectValue: string;
  dataTest?: string;
  inputOnChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
  selectOnChange: (e: React.ChangeEvent<HTMLSelectElement>) => void;
};

export const SelectInputSplitField = ({
  inputType = 'text',
  size = 'full',
  selectOptions,
  placeholder,
  inputDisabled,
  selectDisabled,
  inputValue,
  selectValue,
  inputOnChange,
  selectOnChange,
  dataTest,
}: SelectInputSplitFieldProps) => {
  return (
    <div
      className={clsx('flex rounded', size === 'medium' ? 'w-1/2' : 'w-full')}
    >
      <select
        className="inline-flex form-control"
        style={{
          width: 'max-content',
          borderTopRightRadius: 0,
          borderBottomRightRadius: 0,
          borderRight: 0,
          paddingRight: '1.75rem',
        }}
        disabled={selectDisabled}
        onChange={selectOnChange}
      >
        {selectOptions.map(({ label, value, disabled = false }) => (
          <option
            key={value}
            selected={selectValue === value}
            {...{ value, disabled }}
          >
            {label}
          </option>
        ))}
      </select>

      <input
        type={inputType}
        className={clsx(
          'flex-1 min-w-0 form-control',
          inputDisabled ? 'disabled' : ''
        )}
        style={{
          borderTopLeftRadius: 0,
          borderBottomLeftRadius: 0,
        }}
        data-test={dataTest}
        placeholder={placeholder}
        value={inputValue}
        disabled={inputDisabled}
        onChange={inputOnChange}
      />
    </div>
  );
};
