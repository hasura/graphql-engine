import { Button } from '@/new-components/Button';
import clsx from 'clsx';
import get from 'lodash.get';
import React, { ReactElement } from 'react';
import {
  FieldError,
  FieldPath,
  useFormContext,
  useWatch,
} from 'react-hook-form';
import { BsXCircleFill } from 'react-icons/bs';
import { z, ZodType, ZodTypeDef } from 'zod';
import { FieldWrapper, FieldWrapperPassThroughProps } from './FieldWrapper';

type TFormValues = Record<string, unknown>;

export type Schema = ZodType<TFormValues, ZodTypeDef, TFormValues>;

// putting this into a standalone component so `useWatch` won't have to run even if ClearButton is not enabled.
const ClearButton: React.VFC<{ fieldName: string; className: string }> = ({
  fieldName,
  className,
}) => {
  // typing this as such as we know that it's string:string for this particular case
  const { setValue } = useFormContext<Record<string, string>>();

  // watch the value so we can decide if the clear button should be enabled.
  const value = useWatch<Record<string, string>>({ name: fieldName });

  if (!value) return null;

  return (
    <Button
      className={clsx(
        'border-0 bg-transparent bg-none shadow-none active:opacity-75 pointer-events-auto',
        className
      )}
      onClick={() => setValue(fieldName, '')}
      icon={
        <BsXCircleFill className="!w-4 !h-4 cursor-pointer mr-0 text-gray-300 fill-current hover:text-gray-400" />
      }
    />
  );
};

export type InputFieldProps<T extends z.infer<Schema>> =
  FieldWrapperPassThroughProps & {
    /**
     * The input field name
     */
    name: FieldPath<T>;
    /**
     * The input field type
     */
    type?: 'text' | 'email' | 'password' | 'number' | 'file';
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
    /**
     * A callback for transforming the input onChange for things like sanitizing input
     */
    inputTransform?: (val: string) => string;
    /**
     * Render line breaks in the description
     */
    renderDescriptionLineBreaks?: boolean;
    /**
     * Renders a button to clear the input onClick
     */
    clearButton?: boolean;
    /**
     * The input field classes
     */
    inputClassName?: string;
  };

export const InputField = <T extends z.infer<Schema>>({
  type = 'text',
  name,
  icon,
  iconPosition = 'start',
  placeholder,
  disabled,
  prependLabel = '',
  appendLabel = '',
  dataTest,
  inputTransform,
  renderDescriptionLineBreaks = false,
  clearButton,
  inputClassName,
  ...wrapperProps
}: InputFieldProps<T>) => {
  const {
    register,
    formState: { errors },
  } = useFormContext<T>();

  const maybeError = get(errors, name) as FieldError | undefined;

  const { onChange, ...regReturn } = register(name);
  const showInputEndContainer = clearButton || (iconPosition === 'end' && icon);

  const onInputChange = React.useCallback(
    async event => {
      if (event.target.files?.[0]) {
        onChange(event);
      }
    },
    [onChange]
  );

  return (
    <FieldWrapper
      id={name}
      {...wrapperProps}
      error={maybeError}
      renderDescriptionLineBreaks={renderDescriptionLineBreaks}
    >
      <div className={clsx('flex')}>
        {prependLabel !== '' ? (
          <span className="inline-flex items-center h-input rounded-l text-muted font-semibold px-sm border border-r-0 border-gray-300 bg-gray-50 whitespace-nowrap shadow-sm">
            {prependLabel}
          </span>
        ) : null}
        <div className={clsx('flex relative w-full')}>
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
              'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400 placeholder-gray-500',
              prependLabel !== '' ? 'rounded-l-none' : '',
              appendLabel !== '' ? 'rounded-r-none' : '',
              maybeError
                ? 'border-red-600 hover:border-red-700'
                : 'border-gray-300',
              disabled
                ? 'cursor-not-allowed bg-gray-200 border-gray-200 hover:border-gray-200'
                : 'hover:border-gray-400',
              {
                'pl-10': iconPosition === 'start' && icon,
                'pr-10': iconPosition === 'end' && icon,
              },
              type === 'file' && 'h-auto',
              inputClassName
            )}
            placeholder={placeholder}
            {...regReturn}
            onChange={e => {
              if (inputTransform) {
                e.target.value = inputTransform(e.target.value);
              }
              if (type === 'file') {
                onInputChange(e);
              } else {
                onChange(e);
              }
            }}
            disabled={disabled}
            data-testid={name}
          />
          {showInputEndContainer && (
            <div className="absolute inset-y-0 right-0 flex items-center pointer-events-none">
              {clearButton && (
                <ClearButton
                  fieldName={name}
                  className={clsx(
                    'px-4',
                    iconPosition === 'end' && icon && '-mr-2'
                  )}
                />
              )}

              {iconPosition === 'end' && icon ? (
                <div className={clsx('pr-3 pointer-events-none')}>
                  {React.cloneElement(icon, {
                    className: 'h-5 text-gray-400',
                  })}
                </div>
              ) : null}
            </div>
          )}
        </div>
        {appendLabel !== '' ? (
          <span className="inline-flex items-center h-input rounded-r text-muted font-semibold px-sm border border-l-0 border-gray-300 bg-gray-50 whitespace-nowrap shadow-sm">
            {appendLabel}
          </span>
        ) : null}
      </div>
    </FieldWrapper>
  );
};
