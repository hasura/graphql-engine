import { Switch } from '@hasura/shared/ui';
import { SwitchProps, Flex } from '@radix-ui/themes';
import clsx from 'clsx';
import { useState } from 'react';

type BooleanInputProps = SwitchProps & { initialValue?: boolean };

export const BooleanInput: React.FC<BooleanInputProps> = ({
  name,
  checked,
  onCheckedChange,
  initialValue,
}) => {
  const [value, setValue] = useState(initialValue ?? checked);
  return (
    <div className="block w-full h-input">
      <Flex align="center" justify="center" gap="3" className="h-input grow">
        <label
          htmlFor={name}
          className={clsx(
            'text-muted cursor-pointer',
            !value && 'font-semibold',
          )}
        >
          false
        </label>
        <Switch
          value={value}
          id={name}
          onChange={(isChecked) => {
            setValue(isChecked);
            if (onCheckedChange) {
              onCheckedChange(isChecked);
            }
          }}
        />
        <label
          htmlFor={name}
          className={clsx(
            'text-muted cursor-pointer',
            !!value && 'font-semibold',
          )}
        >
          true
        </label>
      </Flex>
    </div>
  );
};
