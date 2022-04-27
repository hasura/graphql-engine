import clsx from 'clsx';
import React from 'react';

interface TSelect extends React.ComponentProps<'select'> {
  options: string[];
}

export const Select = ({
  options,
  placeholder,
  disabled,
  ...props
}: TSelect) => (
  <select
    {...props}
    className={clsx(
      'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400',
      disabled
        ? 'cursor-not-allowed bg-gray-100 border-gray-100'
        : 'hover:border-gray-400'
    )}
    disabled={disabled}
    value={props.value}
  >
    {placeholder ? (
      <option disabled value="">
        {placeholder}
      </option>
    ) : null}

    {options.map((op, i) => (
      <option value={op} key={i}>
        {op}
      </option>
    ))}
  </select>
);
