import React, { Ref } from 'react';
import {
  Flex,
  FlexProps,
  Text,
  TextProps,
  Checkbox as ThemeCheckbox,
  CheckboxProps as ThemeCheckboxProps,
} from '@radix-ui/themes';
import clsx from 'clsx';

export type CheckedState = ThemeCheckboxProps['checked'];

export type CheckboxProps = Omit<
  ThemeCheckboxProps,
  'ref' | 'onChange' | 'onCheckedChange' | 'checked' | 'value'
> & {
  /**
   * Flag to indicate if the field is invalid
   */
  invalid?: boolean;

  onChange?: ThemeCheckboxProps['onCheckedChange'];
  children?: React.ReactNode;
  value: CheckedState;

  labelProps?: Omit<TextProps, 'as'>;
  align?: FlexProps['align'];
  justify?: FlexProps['justify'];
};

export const Checkbox = React.forwardRef<HTMLButtonElement, CheckboxProps>(
  (
    {
      children,
      invalid,
      color,
      size,
      onChange,
      labelProps = {},
      value,
      align = 'center',
      justify,
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
        <Flex gap="2" align={align} justify={justify}>
          <ThemeCheckbox
            {...rest}
            ref={ref as Ref<HTMLButtonElement>}
            color={invalid ? 'red' : color}
            size={size}
            checked={value}
            onCheckedChange={onChange}
          />
          {children}
        </Flex>
      </Text>
    );
  },
);
