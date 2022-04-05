import { ToolTip } from '@/new-components/Tooltip';
import * as React from 'react';
import clsx from 'clsx';
import { FieldError } from 'react-hook-form';
import { FaExclamationCircle } from 'react-icons/fa';

type FieldWrapperProps = {
  id?: string;
  labelIcon?: React.ReactElement;
  label?: string;
  className?: string;
  children: React.ReactNode;
  error?: FieldError | undefined;
  description?: string;
  tooltip?: string;
  dataTest?: string;
};

export type FieldWrapperPassThroughProps = Omit<
  FieldWrapperProps,
  'className' | 'children' | 'error'
>;

export const FieldWrapper = (props: FieldWrapperProps) => {
  const {
    id,
    labelIcon,
    label,
    className,
    error,
    children,
    description,
    tooltip,
  } = props;
  return (
    <div>
      <label
        htmlFor={id}
        className={clsx('block text-gray-600 mb-xs', className)}
      >
        <span className="flex items-center">
          <span className="font-semibold">
            {labelIcon
              ? React.cloneElement(labelIcon, {
                  className: 'h-4 w-4 mr-xs',
                })
              : null}
            {label}
          </span>
          {tooltip ? <ToolTip message={tooltip} /> : null}
        </span>
        {description ? (
          <span className="text-gray-600 mb-xs">{description}</span>
        ) : null}
      </label>
      <div>{children}</div>
      {error?.message && (
        <div
          role="alert"
          aria-label={error.message}
          className="mt-xs text-red-600 flex items-center"
        >
          <FaExclamationCircle className="fill-current h-4 mr-xs" />
          {error.message}
        </div>
      )}
    </div>
  );
};
