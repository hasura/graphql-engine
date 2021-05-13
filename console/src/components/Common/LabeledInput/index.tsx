import React, { InputHTMLAttributes } from 'react';
import styles from '../../Common/Common.scss';

interface LabeledInputProps extends InputHTMLAttributes<HTMLInputElement> {
  label: string;
  boldlabel?: boolean;
}

export const LabeledInput: React.FC<LabeledInputProps> = props => (
  <>
    <label className={props.boldlabel ? '' : styles.connect_db_input_label}>
      {props?.boldlabel ? <b>{props.label}</b> : props.label}
    </label>
    <input
      type="text"
      className={`form-control ${styles.connect_db_input_pad}`}
      {...props}
    />
  </>
);
