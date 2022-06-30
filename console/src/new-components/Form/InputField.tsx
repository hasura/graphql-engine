import React, { ReactElement } from 'react';
import clsx from 'clsx';
import get from 'lodash.get';
import { FieldError, useFormContext } from 'react-hook-form';
import { FieldWrapper, FieldWrapperPassThroughProps } from './FieldWrapper';

export type InputFieldProps = FieldWrapperPassThroughProps & {
  /**
   * The input field name
   */
  name: string;
  /**
   * The input field type
   */
  type?: 'text' | 'email' | 'password' | 'number';
  /**
   * The input field classes
   */
  className?: string;
  /**
   * The input field icon
   */
  icon?: ReactElement;
  /**
   * The input field icon position
   */
  iconPosition?: 'start' | 'end';
  /**
   * The input field size
   */
  size?: 'full' | 'medium';
  /**
   * The input field placeholder
   */
  placeholder?: string;
  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;
  /**
   * The input field prepend label
   */
  prependLabel?: string;
  /**
   * The input field append label
   */
  appendLabel?: string;
};

export const InputField: React.FC<InputFieldProps> = ({
  type = 'text',
  size = 'full',
  name,
  icon,
  iconPosition = 'start',
  placeholder,
  disabled,
  prependLabel = '',
  appendLabel = '',
  dataTest,
  ...wrapperProps
}: InputFieldProps) => {
  const {
    register,
    formState: { errors },
  } = useFormContext();

  const maybeError = get(errors, name) as FieldError | undefined;
  return (
    <FieldWrapper
      id={name}
      {...wrapperProps}
      className={size === 'medium' ? 'w-1/2 max-w-lg' : 'w-full max-w-xl'}
      error={maybeError}
    >
      <div className={clsx('relative flex')}>
        {prependLabel !== '' ? (
          <span className="inline-flex items-center h-input rounded-l text-muted font-semibold px-sm border border-r-0 border-gray-300 bg-gray-50 whitespace-nowrap shadow-sm">
            {prependLabel}
          </span>
        ) : null}
        {iconPosition === 'start' && icon ? (
          <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
            {React.cloneElement(icon, {
              className: 'h-5 w-5 text-gray-400',
            })}
          </div>
        ) : null}
        <input
          id={name}
          type={type}
          aria-invalid={maybeError ? 'true' : 'false'}
          aria-label={wrapperProps.label}
          data-test={dataTest}
          className={clsx(
            'block w-full max-w-xl h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400 placeholder-gray-500',
            prependLabel !== '' ? 'rounded-l-none' : '',
            appendLabel !== '' ? 'rounded-r-none' : '',
            maybeError
              ? 'border-red-600 hover:border-red-700'
              : 'border-gray-300',
            disabled
              ? 'cursor-not-allowed bg-gray-100 border-gray-100'
              : 'hover:border-gray-400',
            {
              'pl-10': iconPosition === 'start' && icon,
              'pr-10': iconPosition === 'end' && icon,
            }
          )}
          placeholder={placeholder}
          {...register(name)}
          disabled={disabled}
          data-testid={name}
        />
        {iconPosition === 'end' && icon ? (
          <div className="absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none">
            {React.cloneElement(icon, {
              className: 'h-5 text-gray-400',
            })}
          </div>
        ) : null}
        {appendLabel !== '' ? (
          <span className="inline-flex items-center h-input rounded-r text-muted font-semibold px-sm border border-l-0 border-gray-300 bg-gray-50 whitespace-nowrap shadow-sm">
            {appendLabel}
          </span>
        ) : null}
      </div>
    </FieldWrapper>
  );
};
