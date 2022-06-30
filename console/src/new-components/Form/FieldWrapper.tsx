import { ToolTip } from '@/new-components/Tooltip';
import * as React from 'react';
import { FieldError } from 'react-hook-form';
import { FaExclamationCircle } from 'react-icons/fa';

type FieldWrapperProps = {
  /**
   * The field ID
   */
  id?: string;
  /**
   * The field label icon
   */
  labelIcon?: React.ReactElement;
  /**
   * The field label
   */
  label?: string;
  /**
   * The field class
   */
  className?: string;
  /**
   * The field children
   */
  children: React.ReactNode;
  /**
   * The field error
   */
  error?: FieldError | undefined;
  /**
   * The field description
   */
  description?: string;
  /**
   * The field tooltip label
   */
  tooltip?: string;
  /**
   * The field data test id for testing
   */
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
    <div className={className}>
      <label htmlFor={id} className="block text-gray-600 mb-xs">
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
      <div
        role={error?.message ? 'alert' : ''}
        aria-label={error?.message || ''}
        className="text-red-600 flex items-center text-sm mt-1"
      >
        <span className="flex items-center">
          {error?.message && (
            <FaExclamationCircle className="fill-current h-4 mr-xs" />
          )}
          {error?.message}&nbsp;
        </span>
      </div>
    </div>
  );
};
