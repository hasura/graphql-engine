import { Flex, Select as ThemeSelect } from '@radix-ui/themes';
import clsx from 'clsx';
import { forwardRef, ReactNode } from 'react';
import { IconType } from 'react-icons';

export type SelectItemProps = ThemeSelect.ItemProps & {
  label: ReactNode;
};

export type SelectProps = Omit<
  ThemeSelect.RootProps,
  'children' | 'onValueChange'
> & {
  /**
   * Flag to indicate if the field is invalid
   */
  invalid?: boolean;

  /**
   * The placeholder text to display when the field is not valued
   */
  placeholder?: string;

  onChange?: (value: string) => void;

  options: SelectItemProps[];

  icon?: IconType;

  full?: boolean;

  fieldProps?: {
    trigger?: Omit<ThemeSelect.TriggerProps, 'children' | 'placeholder'>;
    content?: Omit<ThemeSelect.ContentProps, 'children'>;
  };
};

export const Select = forwardRef<HTMLButtonElement, SelectProps>(
  (
    {
      options,
      invalid,
      placeholder,
      onChange,
      fieldProps,
      icon: Icon,
      full,
      ...rest
    },
    ref,
  ) => {
    const renderTrigger = () => {
      const {
        color,
        className: triggerClassName,
        ...triggerProps
      } = fieldProps?.trigger ?? {};
      const finalColor = invalid ? 'red' : fieldProps?.trigger?.color;
      const className = clsx(triggerClassName, full ? 'w-full!' : '');

      if (!rest.value && !Icon) {
        return (
          <ThemeSelect.Trigger
            {...triggerProps}
            className={className}
            ref={ref}
            color={finalColor}
            placeholder={placeholder}
          />
        );
      }

      const label =
        (rest.value !== undefined
          ? options.find((opt) => opt.value === rest.value)?.label
          : undefined) ?? placeholder;

      return (
        <ThemeSelect.Trigger
          {...triggerProps}
          className={className}
          color={finalColor}
          placeholder={placeholder}
        >
          {Icon ? (
            <Flex as="span" align="center" gap="2">
              <Icon />
              {label}
            </Flex>
          ) : (
            label
          )}
        </ThemeSelect.Trigger>
      );
    };

    return (
      <ThemeSelect.Root {...rest} onValueChange={onChange}>
        {renderTrigger()}
        <ThemeSelect.Content {...fieldProps?.content}>
          {options.map(({ label, ...option }) => (
            <ThemeSelect.Item {...option} key={option.value}>
              {label}
            </ThemeSelect.Item>
          ))}
        </ThemeSelect.Content>
      </ThemeSelect.Root>
    );
  },
);
