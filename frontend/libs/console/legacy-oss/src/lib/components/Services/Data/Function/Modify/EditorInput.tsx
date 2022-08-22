import React from 'react';

export interface EditorInputProps {
  value: string | number;
  label: string | number;
  placeholder?: string;
  testID?: string;
  onChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
}

const EditorInput: React.FC<EditorInputProps> = ({
  value,
  onChange,
  label,
  placeholder,
  testID,
}) => (
  <div className="flex items-center">
    <label className="text-gray-600 font-semibold">{label}</label>
    <div className="ml-auto w-6/12">
      <input
        className="input-sm form-control"
        value={value}
        onChange={onChange}
        placeholder={placeholder || ''}
        type="text"
        data-test={`${testID}-edit-sessvar-function-field`}
      />
    </div>
  </div>
);

export default EditorInput;
