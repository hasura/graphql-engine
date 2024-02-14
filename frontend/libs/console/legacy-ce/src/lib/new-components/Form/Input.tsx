import clsx from 'clsx';
import React, { ReactElement } from 'react';
import { ChangeHandler, FieldError, RefCallBack } from 'react-hook-form';
import { Button } from '../Button';
import { BsXCircleFill } from 'react-icons/bs';
import { FieldWrapperPassThroughProps } from './FieldWrapper';

type ClearButtonProps = {
  className: string;
  onClick: () => void;
};

const ClearButton: React.VFC<ClearButtonProps> = ({ className, onClick }) => {
  return (
    <Button
      className={clsx(
        'border-0 !bg-transparent bg-none shadow-none active:opacity-75 pointer-events-auto',
        className
      )}
      onClick={() => onClick()}
      icon={
        <BsXCircleFill className="!w-4 !h-4 cursor-pointer mr-0 text-gray-300 fill-current hover:text-gray-400" />
      }
    />
  );
};

export type InputRef = RefCallBack | React.RefObject<HTMLInputElement>;

export type InputProps = FieldWrapperPassThroughProps & {
  /**
   * The input field name
   */
  name: string;
  /**
   * The input field label
   */
  label?: string;
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
  prependLabel?: string | React.ReactNode;
  /**
   * The input field append label
   */
  appendLabel?: string | React.ReactNode;
  /**
   * Renders a button to clear the input onClick
   */
  clearButton?: boolean;
  /**
   * The input field classes
   */
  inputClassName?: string;
  /**
   * If an error is eventually present
   */
  maybeError?: FieldError;
  /**
   * On change event handler
   */
  onChange?: React.ChangeEventHandler<HTMLInputElement>;
  /**
   * On input event handler
   */
  onInput?: React.FormEventHandler<HTMLInputElement>;
  /**
   * Clear button click handler
   */
  onClearButtonClick?: () => void;
  /**
   * Props for the input filed (coming from react-hook-form)
   */
  inputProps?: {
    onBlur?: React.FocusEventHandler<HTMLInputElement> | ChangeHandler;
    ref?: InputRef;
  };
  /**
   * Optional right button
   */
  rightButton?: React.ReactElement;

  /**
   * Custom props to be passed to the HTML input element
   */
  fieldProps?: React.HTMLProps<HTMLInputElement>;
};

export const Input = ({
  type = 'text',
  name,
  icon,
  iconPosition = 'start',
  placeholder,
  disabled,
  prependLabel = '',
  appendLabel = '',
  dataTest,
  dataTestId,
  clearButton,
  inputClassName,
  maybeError,
  onChange,
  onInput,
  onClearButtonClick = () => null,
  inputProps,
  label,
  className = '',
  rightButton,
  fieldProps = {},
}: InputProps) => {
  const showInputEndContainer = clearButton || (iconPosition === 'end' && icon);

  return (
    <div className={clsx('flex', className)}>
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
              role: 'img',
            })}
          </div>
        ) : null}
        <input
          id={name}
          type={type}
          aria-invalid={maybeError ? 'true' : 'false'}
          aria-label={label}
          data-test={dataTest}
          className={clsx(
            'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400 placeholder:text-slate-400',
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
            inputClassName,
            rightButton && 'rounded-r-none border-r-0',
            showInputEndContainer && 'pr-8'
          )}
          placeholder={placeholder}
          {...inputProps}
          onChange={onChange}
          onInput={onInput}
          disabled={disabled}
          data-testid={dataTestId || name}
          onWheelCapture={fieldProps?.onWheelCapture || undefined}
          {...fieldProps}
        />
        {showInputEndContainer && (
          <div className="absolute inset-y-0 right-0 flex items-center pointer-events-none">
            {clearButton && (
              <ClearButton
                className={clsx(
                  'px-4',
                  iconPosition === 'end' && icon && '-mr-2'
                )}
                onClick={onClearButtonClick}
              />
            )}

            {iconPosition === 'end' && icon ? (
              <div className={clsx('pr-3 pointer-events-none')} role="img">
                {React.cloneElement(icon, {
                  className: 'flex h-5 text-gray-400',
                })}
              </div>
            ) : null}
          </div>
        )}
        {rightButton &&
          React.cloneElement(rightButton, {
            className: (rightButton.props?.className || '') + 'rounded-l-none',
          })}
      </div>
      {appendLabel !== '' ? (
        <span className="inline-flex items-center h-input rounded-r text-muted font-semibold px-sm border border-l-0 border-gray-300 bg-gray-50 whitespace-nowrap shadow-sm">
          {appendLabel}
        </span>
      ) : null}
    </div>
  );
};
