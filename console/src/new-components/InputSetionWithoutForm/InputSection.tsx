import React from 'react';
import { IconTooltip } from '../Tooltip';

export const InputSection = (props: {
  label: string;
  value: string;
  onChange: (value: string) => void;
  placeholder: string;
  tooltip: string;
}) => {
  const { label, value, onChange, placeholder, tooltip } = props;
  return (
    <div className="mb-sm">
      <label className="flex items-center mb-xs font-semibold text-muted">
        {label}
        <IconTooltip message={tooltip} />
      </label>
      <input
        type="text"
        value={value}
        onChange={event => onChange(event.target.value)}
        placeholder={placeholder}
        className="block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
      />
    </div>
  );
};
