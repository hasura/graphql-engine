import React, { forwardRef } from 'react';
import {
  CheckboxGroup as ThemeCheckboxGroup,
  Flex,
  Text,
  Skeleton,
  FlexProps,
} from '@radix-ui/themes';

export type CheckboxItem = ThemeCheckboxGroup.ItemProps & {
  label: React.ReactNode;
};

export type CheckboxGroupProps = Omit<
  ThemeCheckboxGroup.RootProps,
  'onChange' | 'onValueChange'
> & {
  /**
   * The options to display with checkboxes
   */
  options: CheckboxItem[];
  /**
   * The checkbox list orientation
   */
  orientation?: 'vertical' | 'horizontal';

  onChange?: (values: string[]) => void;
  gap?: FlexProps['gap'];
  invalid?: boolean;
  loading?: boolean;
};

export const CheckboxGroup = forwardRef<HTMLDivElement, CheckboxGroupProps>(
  (
    {
      options,
      invalid,
      disabled,
      onChange,
      orientation,
      loading = false,
      gap = '2',
      ...rest
    },
    ref,
  ) => {
    return (
      <Skeleton loading={loading}>
        <ThemeCheckboxGroup.Root {...rest} ref={ref} onValueChange={onChange}>
          <Flex
            direction={orientation === 'horizontal' ? 'row' : 'column'}
            gap={gap}
          >
            {options.map(
              ({
                label,
                value,
                disabled: optionDisabled = false,
                ...itemProps
              }) => {
                return (
                  <Text
                    className="cursor-pointer"
                    key={value}
                    as="label"
                    size={rest?.size ?? '2'}
                  >
                    <Flex align="center" gap="2">
                      <ThemeCheckboxGroup.Item
                        {...itemProps}
                        value={value}
                        aria-invalid={invalid ? 'true' : 'false'}
                        disabled={disabled || optionDisabled}
                      />
                      {label}
                    </Flex>
                  </Text>
                );
              },
            )}
          </Flex>
        </ThemeCheckboxGroup.Root>
      </Skeleton>
    );
  },
);

CheckboxGroup.displayName = 'CheckboxGroup';
