import get from 'lodash/get';
import React, { ReactElement } from 'react';
import {
  FieldError,
  FieldPath,
  useFormContext,
  useWatch,
} from 'react-hook-form';
import { z, ZodType } from 'zod';
import { FieldWrapper, FieldWrapperPassThroughProps } from '../FieldWrapper';
import { filterDataAttributes } from '../../utils/filterDataAttributes';
import { LegacyInput } from './LegacyInput';

type TFormValues = Record<string, unknown>;
type Schema = ZodType<TFormValues, TFormValues>;

// for convenience
type FileInputFieldDefaultType = z.infer<Schema>;

// wrappers that want to extend the props in a simple way can use this type
export type ExtendFileInputFieldProps<
  T,
  X extends FileInputFieldDefaultType = FileInputFieldDefaultType,
> = T & FileInputFieldProps<X>;

export type FileInputFieldProps<T extends FileInputFieldDefaultType> =
  FieldWrapperPassThroughProps & {
    /**
     * The input field name
     */
    name: FieldPath<T>;
    /**
     * The input field label
     */
    label?: string;
    /**
     * The input field classes
     */
    className?: string;
    /**
     * The input field icon
     */
    icon?: ReactElement<any>;
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
    prependLabel?: string | React.ReactNode;
    /**
     * The input field append label
     */
    appendLabel?: string | React.ReactNode;
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
    clearable?: boolean;
    /**
     * Handler for when a user presses the clear button, and the state is cleared.
     */
    onClear?: () => void;
    /**
     * The input field classes
     */
    inputClassName?: string;
    rightButton?: ReactElement<any>;
    /**
     * Custom props to be passed to the HTML input element
     */
    fieldProps?: React.HTMLProps<HTMLInputElement>;
  };

export const FileInputField = <T extends z.infer<Schema>>({
  name,
  icon,
  iconPosition = 'start',
  placeholder,
  disabled,
  prependLabel = '',
  appendLabel = '',
  dataTest,
  dataTestId,
  inputTransform,
  renderDescriptionLineBreaks = false,
  clearable,
  onClear,
  inputClassName,
  fieldProps = {},
  ...wrapperProps
}: FileInputFieldProps<T>) => {
  const dataAttributes = filterDataAttributes(
    wrapperProps as Record<string, unknown>,
  );

  const {
    register,
    formState: { errors },
    setValue,
  } = useFormContext<T>();

  const maybeError = get(errors, name) as FieldError | undefined;

  const { onChange, ...regReturn } = register(name, {});

  const value = useWatch<Record<string, string>>({ name });

  const showClearButton = !!value && clearable;

  const onInputChange = React.useCallback(
    async (event: React.ChangeEvent<HTMLInputElement>) => {
      if (event.target.files?.[0]) {
        onChange(event);
      }
    },
    [onChange],
  );

  const onInputChangeEvent = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (inputTransform) {
      e.target.value = inputTransform(e.target.value);
    }
    onInputChange(e);
  };

  const onClearButtonClick = () => {
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    setValue(name, '');
    onClear?.();
  };

  return (
    <FieldWrapper
      id={name}
      {...wrapperProps}
      error={maybeError}
      renderDescriptionLineBreaks={renderDescriptionLineBreaks}
    >
      <LegacyInput
        type="file"
        name={name}
        icon={icon}
        label={wrapperProps?.label || ''}
        iconPosition={iconPosition}
        placeholder={placeholder}
        disabled={disabled}
        prependLabel={prependLabel}
        appendLabel={appendLabel}
        dataTest={dataTest}
        dataTestId={dataTestId}
        clearable={showClearButton}
        inputClassName={inputClassName}
        maybeError={maybeError}
        onChange={onInputChangeEvent}
        onClearButtonClick={onClearButtonClick}
        inputProps={regReturn}
        fieldProps={{ ...fieldProps, ...dataAttributes }}
        rightButton={wrapperProps?.rightButton}
      />
    </FieldWrapper>
  );
};
