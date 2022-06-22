import React, { InputHTMLAttributes } from 'react';
import { ToolTip } from '@/new-components/Tooltip';

interface LabeledInputProps extends InputHTMLAttributes<HTMLInputElement> {
  label: string;
  boldlabel?: boolean;
  tooltipText?: string;
  type?: string;
}

export const LabeledInput: React.FC<LabeledInputProps> = props => (
  <>
    <label
      className={`flex items-center gap-1 ${
        props.boldlabel ? '' : 'inline-block pb-2.5 font-bold'
      }`}
    >
      {props?.boldlabel ? <b>{props.label}</b> : props.label}
      {props.tooltipText && <ToolTip message={props.tooltipText} />}
    </label>
    <input
      type={props?.type ?? 'text'}
      className="form-control font-normal mb-4"
      {...props}
    />
  </>
);
