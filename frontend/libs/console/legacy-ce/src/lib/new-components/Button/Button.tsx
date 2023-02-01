import React, { ReactElement } from 'react';
import { CgSpinner } from 'react-icons/cg';
import clsx from 'clsx';

type ButtonModes = 'default' | 'destructive' | 'primary';
type ButtonSize = 'sm' | 'md' | 'lg';

export interface ButtonProps extends React.ComponentProps<'button'> {
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
    ...otherHtmlAttributes
  } = props;
  const isDisabled = disabled || isLoading;

  const buttonAttributes = {
    type,
    ...otherHtmlAttributes,
    disabled: isDisabled,
    className: clsx(
      sharedButtonStyle,
      buttonModesStyles[mode],
      buttonSizing[size],
      isDisabled ? 'cursor-not-allowed' : '',
      full && fullWidth,
      otherHtmlAttributes?.className
    ),
  };

  if (isLoading) {
    return (
      <button {...buttonAttributes}>
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
      <button {...buttonAttributes}>
        <span className="whitespace-nowrap max-w-full">{children}</span>
      </button>
    );
  }

  return (
    <button {...buttonAttributes}>
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
};

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
