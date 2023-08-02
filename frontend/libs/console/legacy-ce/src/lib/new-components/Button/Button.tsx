import clsx from 'clsx';
import React, { ReactElement } from 'react';
import { CgSpinner } from 'react-icons/cg';

type ButtonModes = 'default' | 'destructive' | 'primary' | 'success';
type ButtonSize = 'sm' | 'md' | 'lg';

export interface ButtonProps
  extends Omit<React.ComponentProps<'button'>, 'ref'> {
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
  loadingText?: React.ReactNode;
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
  // /**
  //  * Will use newer flat style buttons
  //  */
  // appearance?: 'flat' | 'default';
}

export const buttonSizing: Record<ButtonSize, string> = {
  lg: 'px-md py-sm',
  md: 'h-btn px-sm',
  sm: 'h-btnsm px-sm ',
};

const twWhiteBg = `bg-gray-50 from-transparent to-white border-gray-300 hover:border-gray-400 disabled:border-gray-300 focus-visible:from-bg-gray-50 focus-visible:to-bg-gray-50`;

export const twButtonStyles = {
  all: `items-center max-w-full justify-center inline-flex text-sm font-sans font-semibold bg-gradient-to-t border rounded shadow-sm focus-visible:outline-none focus-visible:bg-gradient-to-t focus-visible:ring-2 focus-visible:ring-offset-2 focus-visible:ring-yellow-400 disabled:opacity-60`,
  default: `text-gray-600 ${twWhiteBg} focus-visible:ring-gray-400`,
  destructive: `text-red-600 ${twWhiteBg} focus-visible:ring-red-400`,
  success: `text-green-600 ${twWhiteBg} focus-visible:ring-green-400`,
  primary: `text-gray-600 from-primary to-primary-light border-primary-dark hover:border-primary-darker focus-visible:from-primary focus-visible:to-primary disabled:border-primary-dark`,
};

const fullWidth = 'w-full';

export const Button = React.forwardRef<HTMLButtonElement, ButtonProps>(
  (props, forwardedRef) => {
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

      ...otherHtmlAttributes
    } = props;

    const isDisabled = disabled || isLoading;

    const styles = twButtonStyles;

    const buttonAttributes: typeof props = {
      type,
      ...otherHtmlAttributes,
      onClick: e => {
        if (
          e.target instanceof HTMLElement &&
          e.target.closest('fieldset:disabled')
        ) {
          //this prevents clicks when a fieldset enclosing this button is set to disabled.
          // this is due to a bug in react that's been documented here: https://github.com/facebook/react/issues/7711
          return;
        }
        props?.onClick?.(e);
      },
      disabled: isDisabled,
      className: clsx(
        styles.all,
        styles[mode],
        buttonSizing[size],
        isDisabled ? 'cursor-not-allowed' : '',
        full && fullWidth,
        otherHtmlAttributes?.className
      ),
    };

    if (isLoading) {
      return (
        <button {...buttonAttributes} ref={forwardedRef}>
          {!!loadingText && (
            <span className="whitespace-nowrap mr-2">{loadingText}</span>
          )}
          <CgSpinner
            className={`animate-spin ${size === 'sm' ? 'w-4 h-4' : 'w-5 h-5'}`}
          />
        </button>
      );
    }

    if (!icon) {
      return (
        <button {...buttonAttributes} ref={forwardedRef}>
          <span className="whitespace-nowrap max-w-full">{children}</span>
        </button>
      );
    }

    return (
      <button {...buttonAttributes} ref={forwardedRef}>
        {iconPosition === 'start' && (
          <ButtonIcon
            icon={icon}
            size={size}
            iconPosition={iconPosition}
            buttonHasChildren={!!children}
          />
        )}

        <span className="whitespace-nowrap max-w-full">{children}</span>

        {iconPosition === 'end' && (
          <ButtonIcon
            icon={icon}
            size={size}
            iconPosition={iconPosition}
            buttonHasChildren={!!children}
          />
        )}
      </button>
    );
  }
);

function ButtonIcon(props: {
  size?: ButtonSize;
  icon: ReactElement;
  buttonHasChildren: boolean;
  iconPosition?: 'start' | 'end';
}) {
  const { icon, size, iconPosition, buttonHasChildren } = props;

  const className = clsx(
    'inline-flex',
    {
      'mr-2': buttonHasChildren && iconPosition === 'start',
      'ml-2': buttonHasChildren && iconPosition === 'end',
    },
    size === 'sm' ? 'w-4 h-4' : 'w-5 h-5',
    icon.props.className
  );

  return React.cloneElement(icon, { className });
}
