import {
  Flex,
  RadioGroup as RadixRadioGroup,
  Skeleton,
} from '@radix-ui/themes';
import { forwardRef, Ref, ReactNode } from 'react';

export type RadioGroupItemProps = RadixRadioGroup.ItemProps & {
  label: ReactNode;
};

export type RadioGroupProps = Omit<
  RadixRadioGroup.RootProps,
  'onChange' | 'onValueChange'
> & {
  /**
   * The options to display with radioes
   */
  options: RadioGroupItemProps[];
  /**
   * The radio list orientation
   */
  orientation?: 'vertical' | 'horizontal';

  onChange?: (value: string) => void;
  loading?: boolean;
};

export const RadioGroup = forwardRef(
  (
    {
      onChange,
      options,
      orientation,
      disabled,
      loading = false,
      ...others
    }: RadioGroupProps,
    ref,
  ) => {
    return (
      <Skeleton loading={loading}>
        <Flex
          asChild
          direction={orientation === 'horizontal' ? 'row' : 'column'}
          wrap="wrap"
          gap="4"
        >
          <RadixRadioGroup.Root
            ref={ref as Ref<HTMLDivElement>}
            onValueChange={onChange}
            orientation={orientation}
            {...others}
          >
            {options.map(
              ({ label, disabled: itemDisabled, ...itemProps }, i) => {
                return (
                  <RadixRadioGroup.Item
                    key={i}
                    {...itemProps}
                    disabled={disabled || itemDisabled}
                  >
                    {label}
                  </RadixRadioGroup.Item>
                );
              },
            )}
          </RadixRadioGroup.Root>
        </Flex>
      </Skeleton>
    );
  },
);
