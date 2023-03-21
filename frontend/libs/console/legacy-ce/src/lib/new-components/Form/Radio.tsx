import React, { ReactNode, ReactText } from 'react';
import clsx from 'clsx';
import { v4 as uuid } from 'uuid';
import get from 'lodash/get';
import { FieldError, useFormContext } from 'react-hook-form';
import { FieldWrapper, FieldWrapperPassThroughProps } from './FieldWrapper';

type RadioItem = {
  label: ReactNode;
  value: ReactText;
  disabled?: boolean;
};

export type RadioProps = FieldWrapperPassThroughProps & {
  /**
   * The radio name
   */
  name: string;
  /**
   * The options to display with radioes
   */
  options: RadioItem[];
  /**
   * The radio list orientation
   */
  orientation?: 'vertical' | 'horizontal';
  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;
};

export const Radio: React.FC<RadioProps> = ({
  name,
  options,
  orientation = 'vertical',
  disabled = false,
  dataTest,
  ...wrapperProps
}: RadioProps) => {
  const {
    register,
    formState: { errors },
  } = useFormContext();

  const maybeError = get(errors, name) as FieldError | undefined;
  return (
    <FieldWrapper id={name} {...wrapperProps} error={maybeError}>
      <div
        className={clsx(
          'flex',
          { vertical: 'flex-col', horizontal: 'flex-row gap-6' }[orientation]
        )}
      >
        {options.map(({ label, value, disabled: optionDisabled = false }) => {
          const componentId = `${name}-${uuid()}`;
          return (
            <div key={value} className="flex items-center">
              <input
                type="radio"
                id={componentId}
                value={value}
                aria-invalid={maybeError ? 'true' : 'false'}
                aria-label={wrapperProps.label}
                data-test={dataTest}
                className={clsx(
                  'cursor-pointer rounded-full border shadow-sm m-0',
                  maybeError
                    ? 'border-red-600 hover:border-red-700'
                    : disabled || optionDisabled
                    ? 'cursor-not-allowed bg-gray-200 border-gray-300'
                    : ' border-gray-400 hover:border-gray-500 focus-visible:ring-yellow-400'
                )}
                {...register(name)}
                disabled={disabled || optionDisabled}
                data-testid={`${name}-${value}`}
              />
              <label
                htmlFor={componentId}
                className={clsx(
                  'cursor-pointer m-0 ml-xs',
                  disabled || optionDisabled
                    ? 'text-gray-500 cursor-not-allowed'
                    : ''
                )}
              >
                {label}
              </label>
            </div>
          );
        })}
      </div>
    </FieldWrapper>
  );
};
