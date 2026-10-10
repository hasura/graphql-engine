import { RadioCards } from '@radix-ui/themes';
import { forwardRef, ReactNode } from 'react';

export type RadioCardItemProps = RadioCards.ItemProps & {
  label: ReactNode;
};

export type RadioCardGroupProps = Omit<
  RadioCards.RootProps,
  'onChange' | 'onValueChange'
> & {
  /**
   * The options to display with radio cards
   */
  options: RadioCardItemProps[];

  onChange?: (value: string) => void;
};

export const RadioCardGroup = forwardRef<HTMLDivElement, RadioCardGroupProps>(
  ({ onChange, options, disabled, ...rest }, ref) => {
    return (
      <RadioCards.Root
        disabled={disabled}
        ref={ref}
        {...rest}
        onValueChange={onChange}
      >
        {options.map(({ label, disabled: itemDisabled, ...itemProps }, i) => {
          return (
            <RadioCards.Item
              key={i}
              {...itemProps}
              disabled={disabled || itemDisabled}
            >
              {label}
            </RadioCards.Item>
          );
        })}
      </RadioCards.Root>
    );
  },
);
