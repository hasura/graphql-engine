import React, { InputHTMLAttributes } from 'react';
import { IconTooltip } from '@/new-components/Tooltip';
import { FaExclamationTriangle } from 'react-icons/fa';

interface LabeledInputProps extends InputHTMLAttributes<HTMLInputElement> {
  label: string;
  boldlabel?: boolean;
  tooltipText?: string;
  type?: string;
  icon?: boolean;
}

export const LabeledInput: React.FC<LabeledInputProps> = props => (
  <>
    <label
      className={`flex items-center gap-1 ${
        props.boldlabel ? '' : 'inline-block pb-2.5 font-bold'
      }`}
    >
      {props?.boldlabel ? <b>{props.label}</b> : props.label}
      {props.tooltipText && <IconTooltip message={props.tooltipText} />}
      {props.icon && <FaExclamationTriangle className="text-blue-300 ml-xs" />}
    </label>
    <input
      type={props?.type ?? 'text'}
      className="form-control font-normal mb-4"
      {...props}
    />
  </>
);
