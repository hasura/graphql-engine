import React from 'react';

type Props = {
  name: string;
  label: string;
  placeholder: string;
  value?: string;
  onChange: (value: string) => void;
};

export const FormRow = (props: Props) => {
  const { name, label, placeholder, value = '', onChange } = props;
  return (
    <>
      <label
        htmlFor={name}
        className="font-normal px-sm py-xs text-gray-600 w-1/3"
      >
        {label}
      </label>
      <span className="px-sm py-xs">
        <input
          type="text"
          name={name}
          aria-label={name}
          placeholder={placeholder}
          className="form-control font-normal"
          defaultValue={value}
          onChange={e => onChange(e.target.value)}
        />
      </span>
    </>
  );
};
