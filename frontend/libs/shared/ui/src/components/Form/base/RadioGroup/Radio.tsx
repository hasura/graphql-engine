import {
  Flex,
  Text,
  TextProps,
  Radio as ThemeRadio,
  RadioProps as ThemeRadioProps,
} from '@radix-ui/themes';
import clsx from 'clsx';
import React from 'react';

export type RadioProps = Omit<ThemeRadioProps, 'ref' | 'onChange'> & {
  /**
   * Flag to indicate if the field is invalid
   */
  invalid?: boolean;

  children?: React.ReactNode;

  labelProps?: Omit<TextProps, 'as'>;

  onChange?: ThemeRadioProps['onValueChange'];
};

export const Radio = React.forwardRef<HTMLInputElement, RadioProps>(
  (
    {
      children,
      invalid,
      color,
      size,
      onChange,
      labelProps = {},
      value,
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
        <Flex gap="2" align="center">
          <ThemeRadio
            {...rest}
            ref={ref}
            color={invalid ? 'red' : color}
            size={size}
            value={value}
            onValueChange={onChange}
          />
          {children}
        </Flex>
      </Text>
    );
  },
);
