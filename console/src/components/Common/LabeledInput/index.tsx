import React, { InputHTMLAttributes } from 'react';
import styles from '../../Common/Common.scss';
import Tooltip from '../Tooltip/Tooltip';

interface LabeledInputProps extends InputHTMLAttributes<HTMLInputElement> {
  label: string;
  boldlabel?: boolean;
  tooltipText?: string;
  type?: string;
}

export const LabeledInput: React.FC<LabeledInputProps> = props => (
  <>
    <label className={props.boldlabel ? '' : styles.connect_db_input_label}>
      {props?.boldlabel ? <b>{props.label}</b> : props.label}
      {props.tooltipText && <Tooltip message={props.tooltipText} />}
    </label>
    <input
      type={props?.type ?? 'text'}
      className={`form-control ${styles.connect_db_input_pad}`}
      {...props}
    />
  </>
);
