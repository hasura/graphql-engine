import React, { ReactElement } from 'react';
import { CgSpinner } from 'react-icons/cg';
import clsx from 'clsx';

type ButtonModes = 'default' | 'destructive' | 'primary';
type ButtonSize = 'sm' | 'md';

interface ButtonProps extends React.ComponentProps<'button'> {
  /**
   * Flag indicating whether the button is disabled
   */
  disabled?: boolean;
  /**
   * Flag indicating whether the button is in loading state
   */
  isLoading?: boolean;
  /**
   * The button type
   */
  type?: 'submit' | 'reset' | 'button';
  /**
   * The button mode
   */
  mode?: ButtonModes;
  /**
   * The button size
   */
  size?: ButtonSize;
  /**
   * The button label when in loading state
   */
  loadingText?: string;
  /**
   * The button icon
   */
  icon?: ReactElement;
  /**
   * The button icon position
   */
  iconPosition?: 'start' | 'end';
}

const buttonSizing: Record<ButtonSize, string> = {
  md: 'h-btn px-sm',
  sm: 'h-btnsm px-sm',
};

const buttonModesStyles: Record<ButtonModes, string> = {
  default:
    'text-gray-600 bg-gray-50 from-transparent to-white border-gray-300 hover:border-gray-400 disabled:border-gray-300 focus:from-bg-gray-50 focus:to-bg-gray-50',
  destructive:
    'text-red-600 bg-gray-50 from-transparent to-white border-gray-300 hover:border-gray-400 disabled:border-gray-300 focus:from-bg-gray-50 focus:to-bg-gray-50',
  primary:
    'text-gray-600 from-primary to-primary-light border-primary-dark hover:border-primary-darker focus:from-primary focus:to-primary disabled:border-primary-dark',
};

const sharedButtonStyle =
  'inline-flex space-x-2 items-center font-semibold bg-gradient-to-t border rounded shadow-sm focus:outline-none focus:bg-gradient-to-t focus:ring-2 focus:ring-offset-2 focus:ring-yellow-400 disabled:opacity-60';

export const Button = (props: ButtonProps) => {
  const {
    mode = 'default',
    type = 'button',
    size = 'md',
    children,
    icon,
    iconPosition = 'start',
    isLoading,
    loadingText,
    disabled,
    ...rest
  } = props;
  const isDisabled = disabled || isLoading;

  // Primary button is only available in md size
  const buttonSize = mode === 'primary' ? 'md' : size;

  return (
    <button
      type={type}
      className={clsx(
        sharedButtonStyle,
        buttonSizing[buttonSize],
        buttonModesStyles[mode],
        isDisabled ? 'cursor-not-allowed' : ''
      )}
      {...rest}
      disabled={isDisabled}
    >
      {isLoading ? (
        <>
          {loadingText ? (
            <span className="whitespace-nowrap">{loadingText}</span>
          ) : null}
          <CgSpinner
            className={`animate-spin ${
              buttonSize === 'sm' ? 'w-4 h-4' : 'w-5 h-5'
            }`}
          />
        </>
      ) : (
        <>
          {icon && iconPosition === 'start'
            ? React.cloneElement(icon, {
                className: `inline-flex ${
                  buttonSize === 'sm' ? 'w-4 h-4' : 'w-5 h-5'
                }`,
              })
            : null}
          <span className="whitespace-nowrap">{children}</span>
          {icon && iconPosition === 'end'
            ? React.cloneElement(icon, {
                className: `inline-flex ${
                  buttonSize === 'sm' ? 'w-4 h-4' : 'w-5 h-5'
                }`,
              })
            : null}
        </>
      )}
    </button>
  );
};
