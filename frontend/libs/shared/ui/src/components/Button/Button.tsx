import clsx from 'clsx';
import React from 'react';
import {
  Button as ThemeButton,
  Spinner,
  type ButtonProps as ThemeButtonProps,
  Flex,
  Text,
} from '@radix-ui/themes';
import { IconType } from 'react-icons';
import ButtonIcon from './InternalButtonIcon';
import { buttonModeColors, ButtonModes } from './utils';

export type ButtonSize = 'sm' | 'md' | 'lg';

export interface ButtonProps extends Omit<ThemeButtonProps, 'size' | 'ref'> {
  /**
   * The button mode
   */
  mode?: ButtonModes;
  /**
   * The button size
   */
  size?: ButtonSize | ThemeButtonProps['size'];
  /**
   * The button label when in loading state
   */
  loadingText?: React.ReactNode;
  /**
   * The left button icon
   */
  leftIcon?: IconType | null;
  /**
   * The right button icon
   */
  rightIcon?: IconType | null;

  iconClassName?: string;

  full?: boolean;
}

export const buttonSizes: Record<ButtonSize, ThemeButtonProps['size']> = {
  sm: '1',
  md: '2',
  lg: '3',
};

export const Button = React.forwardRef<HTMLButtonElement, ButtonProps>(
  (props, forwardedRef) => {
    const {
      mode,
      size = '2',
      children,
      leftIcon,
      rightIcon,
      loading,
      loadingText,
      disabled,
      iconClassName,
      full,
      type = 'button',
      ...otherHtmlAttributes
    } = props;

    const isDisabled = disabled || loading;
    const modeProps = mode ? buttonModeColors[mode] : undefined;

    const buttonAttributes = {
      ...modeProps,
      ...otherHtmlAttributes,
      type,
      onClick: (e: React.MouseEvent<HTMLButtonElement>) => {
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
      size:
        (size as ButtonSize) in buttonSizes
          ? buttonSizes[size as ButtonSize]
          : (size as ThemeButtonProps['size']),
      className: clsx(
        full ? 'w-full!' : '',
        isDisabled ? 'cursor-not-allowed' : '',
        otherHtmlAttributes?.className,
      ),
    };

    if (loading) {
      return (
        <ThemeButton {...buttonAttributes} ref={forwardedRef}>
          {loadingText ? (
            <Flex gap="2" align="center">
              <Spinner />
              <Text className="whitespace-nowrap" size="2" as="span">
                {loadingText}
              </Text>
            </Flex>
          ) : (
            <Spinner />
          )}
        </ThemeButton>
      );
    }

    if (!leftIcon && !rightIcon) {
      return (
        <ThemeButton {...buttonAttributes} ref={forwardedRef}>
          <span className="whitespace-nowrap max-w-full">{children}</span>
        </ThemeButton>
      );
    }

    return (
      <ThemeButton {...buttonAttributes} ref={forwardedRef}>
        {leftIcon ? (
          <ButtonIcon
            className={iconClassName}
            icon={leftIcon}
            iconPosition="start"
            buttonHasChildren={!!children}
            size={size}
          />
        ) : null}

        <span className="whitespace-nowrap max-w-full">{children}</span>

        {rightIcon ? (
          <ButtonIcon
            className={iconClassName}
            icon={rightIcon}
            iconPosition="end"
            buttonHasChildren={!!children}
            size={size}
          />
        ) : null}
      </ThemeButton>
    );
  },
);

Button.displayName = 'Button';
