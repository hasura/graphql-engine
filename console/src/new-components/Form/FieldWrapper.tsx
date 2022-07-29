import { ToolTip } from '@/new-components/Tooltip';
import * as React from 'react';
import { FieldError } from 'react-hook-form';
import clsx from 'clsx';
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
   * The field size (full: the full width of the container , medium: half the
   * width of the container)
   */
  size?: 'full' | 'medium';
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
   * Flag indicating wheteher the field is horizontally aligned
   */
  horizontal?: boolean;
  /**
   * The field data test id for testing
   */
  dataTest?: string;
};

export type FieldWrapperPassThroughProps = Omit<
  FieldWrapperProps,
  'className' | 'children' | 'error'
>;

const ErrorComponentTemplate = (props: {
  label: React.ReactNode;
  ariaLabel?: string;
  role?: string;
}) => {
  const ariaAttributes: {
    'aria-label'?: string;
    role?: string;
  } = {};

  if (props.role && props.ariaLabel) {
    ariaAttributes.role = props.role;
    ariaAttributes['aria-label'] = props.ariaLabel;
  }

  return (
    <div
      {...ariaAttributes}
      className="text-red-600 flex items-center text-sm mt-1"
    >
      <span className="flex items-center">{props.label}</span>
    </div>
  );
};

export const FieldWrapper = (props: FieldWrapperProps) => {
  const {
    id,
    labelIcon,
    label,
    className,
    size = 'full',
    error,
    children,
    description,
    tooltip,
    horizontal,
  } = props;
  return (
    <div
      className={clsx(
        className,
        'max-w-screen-md',
        size === 'medium' ? 'w-1/2' : 'w-full',
        horizontal && 'flex flex-row flex-wrap w-full justify-between'
      )}
    >
      <label
        htmlFor={id}
        className={clsx(
          'block pt-1 text-gray-600 mb-xs',
          horizontal && 'pr-8 flex-grow220px'
        )}
      >
        <span
          className={clsx(
            'flex items-center',
            horizontal ? 'text-muted' : 'font-semibold'
          )}
        >
          <span>
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
          <span className="text-gray-600 mb-xs font-normal text-sm">
            {description}
          </span>
        ) : null}
      </label>
      <div className={clsx(horizontal && 'flex-grow320px')}>
        <div>{children}</div>
        {error ? (
          <ErrorComponentTemplate
            label={
              <>
                <FaExclamationCircle className="fill-current h-4 w-4 mr-xs flex-shrink-0" />
                {props.error?.message}
              </>
            }
            ariaLabel={props.error?.message ?? ''}
            role="alert"
          />
        ) : (
          /* A &nbsp; character is displayed even if there is no error to
          book some space for the error message. It prevents other fields to
          be pushed down when an error is displayed. */
          <ErrorComponentTemplate label={<>&nbsp;</>} />
        )}
      </div>
    </div>
  );
};
