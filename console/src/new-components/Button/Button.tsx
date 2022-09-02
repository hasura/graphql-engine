import React, { ReactElement } from 'react';
import { CgSpinner } from 'react-icons/cg';
import clsx from 'clsx';

type ButtonModes = 'default' | 'destructive' | 'primary';
type ButtonSize = 'sm' | 'md' | 'lg';

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
  /**
   * The button will take the maximum with possible
   */
  full?: boolean;
}

const buttonSizing: Record<ButtonSize, string> = {
  lg: 'px-md py-sm',
  md: 'h-btn px-sm',
  sm: 'h-btnsm px-sm ',
};

const buttonModesStyles: Record<ButtonModes, string> = {
  default:
    'text-gray-600 bg-gray-50 from-transparent to-white border-gray-300 hover:border-gray-400 disabled:border-gray-300 focus-visible:from-bg-gray-50 focus-visible:to-bg-gray-50',
  destructive:
    'text-red-600 bg-gray-50 from-transparent to-white border-gray-300 hover:border-gray-400 disabled:border-gray-300 focus-visible:from-bg-gray-50 focus-visible:to-bg-gray-50',
  primary:
    'text-gray-600 from-primary to-primary-light border-primary-dark hover:border-primary-darker focus-visible:from-primary focus-visible:to-primary disabled:border-primary-dark',
};

const sharedButtonStyle =
  'items-center max-w-full justify-center inline-flex items-center text-sm font-sans font-semibold bg-gradient-to-t border rounded shadow-sm focus-visible:outline-none focus-visible:bg-gradient-to-t focus-visible:ring-2 focus-visible:ring-offset-2 focus-visible:ring-yellow-400 disabled:opacity-60';

const fullWidth = 'w-full';

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
    full,
    ...rest
  } = props;
  const isDisabled = disabled || isLoading;

  if (
    mode === 'primary' &&
    size === 'sm' &&
    process.env.mode !== 'production'
  ) {
    console.warn(
      '%cPrimary buttons should not be used with small size',
      'font-weight: bold; font-size: 50px; color: red; text-shadow: 3px 3px 0 rgb(217, 31, 38) , 6px 6px 0 rgb(226, 91, 14), 9px 9px 0 rgb(245, 221, 8), 12px 12px 0 rgb(5, 148, 68), 15px 15px 0 rgb(2, 135, 206), 18px 18px 0 rgb(4, 77, 245), 21px 21px 0 rgb(42, 21, 113); line-height: 2.5;padding-right: 20px'
    );
  }

  return (
    <button
      type={type}
      {...rest}
      className={clsx(
        sharedButtonStyle,
        buttonModesStyles[mode],
        buttonSizing[size],
        isDisabled ? 'cursor-not-allowed' : '',
        full && fullWidth,
        rest?.className
      )}
      disabled={isDisabled}
    >
      {isLoading ? (
        <>
          {loadingText ? (
            <span className="whitespace-nowrap mr-2">{loadingText}</span>
          ) : null}
          <CgSpinner
            className={`animate-spin ${size === 'sm' ? 'w-4 h-4' : 'w-5 h-5'}`}
          />
        </>
      ) : (
        <>
          {icon && iconPosition === 'start'
            ? React.cloneElement(icon, {
                className: `inline-flex ${children && 'mr-2'} ${
                  size === 'sm' ? 'w-4 h-4' : 'w-5 h-5'
                }`,
              })
            : null}
          <span className="whitespace-nowrap max-w-full">{children}</span>
          {icon && iconPosition === 'end'
            ? React.cloneElement(icon, {
                className: `inline-flex ${children && 'ml-2'} ${
                  size === 'sm' ? 'w-4 h-4' : 'w-5 h-5'
                }`,
              })
            : null}
        </>
      )}
    </button>
  );
};
