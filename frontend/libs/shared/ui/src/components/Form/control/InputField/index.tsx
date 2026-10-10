import React from 'react';
import { Controller, FieldPath, useFormContext } from 'react-hook-form';
import { z, ZodType } from 'zod';
import { FieldWrapper, FieldWrapperPassThroughProps } from '../FieldWrapper';
import { Input, InputProps } from '../../base';

type TFormValues = Record<string, unknown>;

export type Schema = ZodType<TFormValues, TFormValues>;

// for convenience
type InputFieldDefaultType = z.infer<Schema>;

// wrappers that want to extend the props in a simple way can use this type
export type ExtendInputFieldProps<
  T,
  X extends InputFieldDefaultType = InputFieldDefaultType,
> = T & InputFieldProps<X>;

export type InputFieldProps<T extends InputFieldDefaultType> = Omit<
  FieldWrapperPassThroughProps,
  'fieldProps'
> & {
  className?: string;
  /**
   * The input field name
   */
  name: FieldPath<T>;
  /**
   * A callback for transforming the input onChange for things like sanitizing input
   */
  inputTransform?: (val: string) => string;
  /**
   * Render line breaks in the description
   */
  renderDescriptionLineBreaks?: boolean;
  /**
   * Custom props to be passed to the HTML input element
   */
  fieldProps?: Omit<InputProps, 'name' | 'className'>;
};

export const InputField = <T extends z.infer<Schema>>({
  name,
  dataTest,
  dataTestId,
  inputTransform,
  renderDescriptionLineBreaks = false,
  fieldProps,
  label,
  ...wrapperProps
}: InputFieldProps<T>) => {
  const { control, setValue } = useFormContext<T>();

  const onClearButtonClick = () => {
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    setValue(name, '');
    fieldProps?.onClear?.();
  };

  return (
    <Controller
      name={name}
      control={control}
      render={({ field: { onChange, value, ...field }, fieldState }) => {
        const showClearButton = !!value && fieldProps?.clearable;

        return (
          <FieldWrapper
            {...wrapperProps}
            id={name}
            label={label!}
            error={fieldState.error}
            dataTest={dataTest}
            dataTestId={dataTestId}
            renderDescriptionLineBreaks={renderDescriptionLineBreaks}
          >
            <Input
              {...fieldProps}
              {...field}
              clearable={showClearButton}
              isInvalid={fieldState.invalid}
              onChange={(e: React.ChangeEvent<HTMLInputElement>) => {
                if (inputTransform) {
                  e.target.value = inputTransform(e.target.value);
                }

                // Mirror `register(name, { valueAsNumber: true })`: number
                // inputs store a number (NaN when empty), which is what the
                // forms' zod schemas expect.
                if (fieldProps?.type === 'number') {
                  onChange(e.target.valueAsNumber);
                  return;
                }

                onChange(e);
              }}
              onClear={onClearButtonClick}
              value={
                fieldProps?.type === 'number' && Number.isNaN(value)
                  ? ''
                  : (value as any)
              }
            />
          </FieldWrapper>
        );
      }}
    />
  );
};
