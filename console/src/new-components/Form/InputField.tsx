import React, { ReactElement } from 'react';
import clsx from 'clsx';
import get from 'lodash.get';
import { FieldError, useFormContext } from 'react-hook-form';

import { FieldWrapper, FieldWrapperPassThroughProps } from './FieldWrapper';

export type InputFieldProps = FieldWrapperPassThroughProps & {
  name: string;
  type?: 'text' | 'email' | 'password';
  className?: string;
  icon?: ReactElement;
  iconPosition?: 'start' | 'end';
  size?: 'full' | 'medium';
  placeholder?: string;
  disabled?: boolean;
};

export const InputField = ({
  type = 'text',
  size = 'full',
  name,
  icon,
  iconPosition = 'start',
  placeholder,
  disabled,
  ...wrapperProps
}: InputFieldProps) => {
  const {
    register,
    formState: { errors },
  } = useFormContext();

  const maybeError = get(errors, name) as FieldError | undefined;

  return (
    <FieldWrapper {...wrapperProps} error={maybeError}>
      <div
        className={clsx(
          'relative',
          size === 'medium' ? 'w-1/2 max-w-lg' : 'w-full max-w-xl'
        )}
      >
        {iconPosition === 'start' && icon ? (
          <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
            {React.cloneElement(icon, {
              className: 'h-5 w-5 text-gray-400',
            })}
          </div>
        ) : null}
        <input
          type={type}
          aria-invalid={maybeError ? 'true' : 'false'}
          className={clsx(
            'block h-input w-full shadow-sm rounded focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400',
            maybeError
              ? 'border-red-600 hover:border-red-700 placeholder-red-600 '
              : 'border-gray-300 placeholder-gray-600',
            disabled
              ? 'cursor-not-allowed bg-gray-100 border-gray-100'
              : 'hover:border-gray-400',
            {
              'pl-10': iconPosition === 'start' && icon,
              'pr-10': iconPosition === 'end' && icon,
            }
          )}
          placeholder={placeholder}
          {...register(name, { disabled })}
        />
        {iconPosition === 'end' && icon ? (
          <div className="absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none">
            {React.cloneElement(icon, {
              className: 'h-5 text-gray-400',
            })}
          </div>
        ) : null}
      </div>
    </FieldWrapper>
  );
};
