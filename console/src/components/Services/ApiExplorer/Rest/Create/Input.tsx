import React from 'react';
import styles from '../RESTStyles.scss';

interface InputProps extends React.ComponentProps<'input'> {
  label: string;
  onChangeText: (v: string) => void;
  note?: string;
  type?: 'text' | 'textarea';
}

const Input: React.FC<InputProps> = ({
  label,
  value,
  onChangeText,
  placeholder,
  type = 'text',
  className,
  note,
  ...props
}) => {
  const onInputChange = (
    e: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>
  ) => {
    const currentValue = e.target.value;
    onChangeText(currentValue);
  };

  if (type === 'textarea') {
    return (
      <>
        <label
          className={styles.form_input_label}
          htmlFor={`rest-input-${label}`}
        >
          {label}
        </label>
        <textarea
          rows={3}
          onChange={onInputChange}
          placeholder={placeholder}
          value={value}
          id={`rest-input-${label}`}
          className={`form-control ${styles.input_width_50} ${
            styles.margin_input
          } ${className ?? ''}`}
        />
      </>
    );
  }

  return (
    <div className={styles.create_input_layout}>
      <div className={styles.input_width_50}>
        <label
          className={styles.form_input_label}
          htmlFor={`rest-input-${label}`}
        >
          {label}
        </label>
        <input
          type="text"
          onChange={onInputChange}
          placeholder={placeholder}
          value={value}
          id={`rest-input-${label}`}
          className={`form-control ${styles.margin_input} ${className ?? ''}`}
        />
        {props?.children ?? null}
      </div>
      <div className={styles.input_note}>{note && <p>{note}</p>}</div>
    </div>
  );
};

export default Input;
