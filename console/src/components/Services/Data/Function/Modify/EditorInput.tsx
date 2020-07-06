import React from 'react';
import styles from './ModifyCustomFunction.scss';

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
  <div className={`${styles.display_flex} form-group`}>
    <label className="col-xs-4">{label}</label>
    <div className="col-xs-6">
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
