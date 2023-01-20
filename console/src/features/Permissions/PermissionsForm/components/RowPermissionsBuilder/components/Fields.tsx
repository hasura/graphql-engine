import React from 'react';
import { JsonItem } from './Elements';

const Input = React.forwardRef<HTMLInputElement, React.ComponentProps<'input'>>(
  (props, ref) => {
    return (
      <div className="flex gap-1">
        {props.type === 'text' && <JsonItem text={`"`} />}
        <label
          htmlFor={props.id}
          className="block text-sm font-medium text-gray-700"
        >
          {/* <strong>{props.title}</strong> */}
          <input
            ref={ref}
            className="block w-36 h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
            {...props}
          />
        </label>
        {props.type === 'text' && <JsonItem text={`"`} />}
      </div>
    );
  }
);

const Select = (props: React.ComponentProps<'select'>) => {
  return (
    <div className="flex gap-1">
      <JsonItem text={`"`} />
      <label
        htmlFor={props.id}
        className="block text-sm font-medium text-gray-700"
      >
        {/* <strong>{props.title}</strong> */}
        <select
          className="block w-32 h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
          {...props}
        />
      </label>
      <JsonItem text={`"`} />
    </div>
  );
};

export const CustomField = {
  Input,
  Select,
};
