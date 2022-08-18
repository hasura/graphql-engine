import React from 'react';

export interface TextInputProps extends React.ComponentProps<'input'> {
  bsclass?: string;
  onChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
}

const TextInput: React.FC<TextInputProps> = ({
  type = 'text',
  placeholder = '',
  bsclass = '',
  onChange,
  ...rest
}) => {
  return (
    <input
      {...rest}
      onChange={onChange}
      type={type}
      placeholder={placeholder}
      className={`${bsclass} form-control`}
    />
  );
};

export default TextInput;
