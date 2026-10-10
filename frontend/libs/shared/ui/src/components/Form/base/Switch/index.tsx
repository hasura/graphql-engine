import {
  Flex,
  Skeleton,
  Text,
  TextProps,
  Switch as ThemeSwitch,
  SwitchProps as ThemeSwitchProps,
} from '@radix-ui/themes';
import clsx from 'clsx';
import React from 'react';

export type SwitchProps = Omit<
  ThemeSwitchProps,
  'ref' | 'onChange' | 'onCheckedChange' | 'checked' | 'value'
> & {
  /**
   * Flag to indicate if the field is invalid
   */
  invalid?: boolean;

  onChange?: ThemeSwitchProps['onCheckedChange'];
  children?: React.ReactNode;
  value?: boolean;
  loading?: boolean;

  labelProps?: Omit<TextProps, 'as'>;
};

export const Switch = React.forwardRef<HTMLButtonElement, SwitchProps>(
  (
    {
      children,
      invalid,
      color,
      size,
      onChange,
      labelProps = {},
      value,
      loading = false,
      ...rest
    },
    ref,
  ) => {
    const {
      className: labelClassName,
      color: labelColor,
      ...labelRest
    } = labelProps;

    return (
      <Skeleton loading={loading}>
        <Text
          as="label"
          className={clsx(
            rest.disabled ? 'cursor-not-allowed' : 'cursor-pointer',
            labelClassName,
          )}
          color={rest.disabled ? 'gray' : invalid ? 'red' : labelColor}
          size={size ?? '2'}
          {...(labelRest as TextProps)}
        >
          <Flex gap="2" align="center">
            <ThemeSwitch
              {...rest}
              ref={ref}
              color={invalid ? 'red' : color}
              size={size}
              checked={value}
              onCheckedChange={onChange}
            />
            {children}
          </Flex>
        </Text>
      </Skeleton>
    );
  },
);
