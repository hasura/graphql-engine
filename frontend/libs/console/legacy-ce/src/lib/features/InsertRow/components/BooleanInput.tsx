import { Switch } from '../../../new-components/Switch';
import { SwitchProps } from '@radix-ui/react-switch';
import clsx from 'clsx';
import { useState } from 'react';

type BooleanInputProps = SwitchProps;

export const BooleanInput: React.VFC<BooleanInputProps> = ({
  name,
  checked,
  onCheckedChange,
}) => {
  const [value, setValue] = useState(checked);
  return (
    <div className="block w-full h-input">
      <div className="flex h-input items-center justify-center flex-grow gap-3">
        <label
          htmlFor={name}
          className={clsx(
            'text-muted cursor-pointer',
            !value && 'font-semibold'
          )}
        >
          false
        </label>
        <Switch
          checked={value}
          id={name}
          onCheckedChange={isChecked => {
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
            !!value && 'font-semibold'
          )}
        >
          true
        </label>
      </div>
    </div>
  );
};
