import clsx from 'clsx';
import React, { Ref } from 'react';
import { Flex, Text, TextField } from '@radix-ui/themes';
import ClearButton from './ClearButton';
import { IconType } from 'react-icons';

export type InputProps = TextField.RootProps & {
  /**
   * The input field classes
   */
  containerClassName?: string;
  /**
   * The input field icon
   */
  icon?: IconType;
  /**
   * The input field icon position
   */
  iconPosition?: 'start' | 'end';
  /**
   * The input field prepend label
   */
  prependLabel?: string | React.ReactNode;
  /**
   * The input field append label
   */
  appendLabel?: string | React.ReactNode;
  /**
   * If an error is eventually present
   */
  isInvalid?: boolean;
  /**
   * Renders a button to clear the input onClick
   */
  clearable?: boolean;
  /**
   * Clear button click handler
   */
  onClear?: () => void;
  /**
   * Optional right button
   */
  rightButton?: React.ReactElement<any>;

  /**
   * Stretch the input to the full row.
   */
  full?: boolean;
};

const edgeStyles = {
  color: 'gray' as const,
  className:
    'inline-flex items-center px-3 border border-gray-300 bg-gray-50 dark:border-slate-600 dark:bg-slate-800 dark:text-slate-200 whitespace-nowrap',
};

export const Input = React.forwardRef(
  (
    {
      containerClassName,
      icon: Icon,
      iconPosition = 'start',
      disabled,
      prependLabel,
      appendLabel,
      clearable,
      isInvalid,
      onChange,
      onInput,
      onClear,
      rightButton,
      className,
      size,
      radius,
      full,
      ...props
    }: InputProps,
    ref,
  ) => {
    const showInputEndContainer = clearable || (iconPosition === 'end' && Icon);
    const handleClear = () => {
      if (onClear) {
        return onClear();
      }

      const handle = onChange || onInput;
      if (handle) {
        return () =>
          handle({
            target: {
              value: '',
            },
          } as unknown as React.ChangeEvent<
            HTMLInputElement,
            HTMLInputElement
          > &
            React.InputEvent<HTMLInputElement>);
      }
    };

    return (
      <Flex className={clsx(containerClassName, full ? 'w-full' : undefined)}>
        {!prependLabel ? null : typeof prependLabel === 'string' ? (
          <Text
            size={size ?? '2'}
            color={edgeStyles.color}
            className={clsx(edgeStyles.className, 'rounded-l border-r-0')}
          >
            {prependLabel}
          </Text>
        ) : (
          prependLabel
        )}
        <Flex className="relative w-full">
          <TextField.Root
            {...props}
            ref={ref as Ref<HTMLInputElement>}
            aria-invalid={isInvalid ? 'true' : 'false'}
            color={
              isInvalid ? ('red' as TextField.RootProps['color']) : undefined
            }
            className={clsx(
              'w-full',
              prependLabel ? '' : 'rounded-l-none',
              appendLabel || rightButton ? '' : 'rounded-r-none',
              disabled ? 'cursor-not-allowed' : '',
              rightButton && 'border-r-0',
              className,
            )}
            size={size}
            radius={
              prependLabel || appendLabel || rightButton ? 'none' : radius
            }
            onChange={onChange}
            onInput={onInput}
            disabled={disabled}
          >
            {iconPosition === 'start' && Icon ? (
              <TextField.Slot side="left">
                <Icon />
              </TextField.Slot>
            ) : null}
            {showInputEndContainer && (
              <TextField.Slot side="right">
                {clearable && (
                  <ClearButton
                    className={clsx({
                      '-mr-2': iconPosition === 'end' && Icon,
                    })}
                    onClick={handleClear}
                  />
                )}
                {iconPosition === 'end' && Icon ? <Icon /> : null}
              </TextField.Slot>
            )}
          </TextField.Root>
          {rightButton}
        </Flex>
        {!appendLabel ? null : typeof appendLabel === 'string' ? (
          <Text
            size={size ?? '2'}
            color={edgeStyles.color}
            className={clsx(edgeStyles.className, 'rounded-r border-l-0')}
          >
            {appendLabel}
          </Text>
        ) : (
          appendLabel
        )}
      </Flex>
    );
  },
);
