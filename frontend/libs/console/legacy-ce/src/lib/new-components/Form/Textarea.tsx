import React from 'react';
import clsx from 'clsx';
import get from 'lodash/get';
import { FieldError, useFormContext } from 'react-hook-form';
import { FieldWrapper, FieldWrapperPassThroughProps } from './FieldWrapper';

export type TextareaProps = FieldWrapperPassThroughProps & {
  /**
   * The textarea name
   */
  name: string;
  /**
   * The textarea visible rows number
   */
  rowsNumber?: number;
  /**
   * The textarea placeholder
   */
  placeholder?: string;
  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;
};

export const Textarea: React.FC<TextareaProps> = ({
  rowsNumber = 3,
  name,
  placeholder,
  disabled,
  dataTest,
  ...wrapperProps
}: TextareaProps) => {
  const {
    register,
    formState: { errors },
  } = useFormContext();

  const maybeError = get(errors, name) as FieldError | undefined;
  return (
    <FieldWrapper id={name} {...wrapperProps} error={maybeError}>
      <div className={clsx('relative')}>
        <textarea
          id={name}
          rows={rowsNumber}
          aria-invalid={maybeError ? 'true' : 'false'}
          aria-label={wrapperProps.label}
          data-test={dataTest}
          className={clsx(
            'block w-full shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400 placeholder-gray-500',
            maybeError
              ? 'border-red-600 hover:border-red-700'
              : 'border-gray-300',
            disabled
              ? 'cursor-not-allowed bg-gray-200 border-gray-200'
              : 'hover:border-gray-400'
          )}
          placeholder={placeholder}
          {...register(name)}
          disabled={disabled}
          data-testid={name}
        />
      </div>
    </FieldWrapper>
  );
};
