import React, { PropsWithChildren } from 'react';
import { v4 as uuid } from 'uuid';
import { FaCheck, FaMinus } from 'react-icons/fa';

import * as RadixCheckbox from '@radix-ui/react-checkbox';
import clsx from 'clsx';

type CheckboxProps = PropsWithChildren<{
  /**
   * The checkbox id
   */
  id?: string;
  /**
   * The checkbox name
   */
  name?: string;
  /**
   * The checkbox class name
   */
  className?: string;
  /**
   * Flag to indicate if the checkbox is checked
   */
  checked?: RadixCheckbox.CheckedState;
  /**
   * Flag to indicate if the checkbox is default checked
   */
  defaultChecked?: RadixCheckbox.CheckedState;
  /**
   * Handler for the checked change event
   */
  onCheckedChange?: (event: RadixCheckbox.CheckedState) => void;
  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;
  /**
   * Flag to indicate if the field is invalid
   */
  invalid?: boolean;
  /**
   * The checkbox options
   */
  options?: {
    root: RadixCheckbox.CheckboxProps;
    indicator: RadixCheckbox.CheckboxIndicatorProps;
  };
}>;

export const Checkbox: React.FC<CheckboxProps> = props => {
  const {
    id,
    children,
    name,
    disabled,
    invalid,
    className,
    checked,
    defaultChecked,
    onCheckedChange,
    options,
  } = props;
  const componentId = id ? id : `${name}-${uuid()}`;
  const [internalCheckedState, setChecked] = React.useState<
    RadixCheckbox.CheckedState | undefined
  >(undefined);

  React.useEffect(() => {
    if (defaultChecked !== undefined) {
      setChecked(defaultChecked);
    }
  }, []);

  React.useEffect(() => {
    if (checked !== undefined) {
      setChecked(checked);
    }
  }, [checked]);

  const onCheckedChangeLocal = (
    checkedFromEvent: RadixCheckbox.CheckedState
  ) => {
    setChecked(checkedFromEvent);
    if (onCheckedChange !== undefined) {
      onCheckedChange(checkedFromEvent);
    }
  };

  return (
    <div className={clsx(className, 'flex items-start pr-sm')}>
      <RadixCheckbox.Root
        {...options?.root}
        id={componentId}
        checked={internalCheckedState}
        onCheckedChange={onCheckedChangeLocal}
        className={clsx(
          'w-4 h-4 relative mt-1 cursor-pointer rounded border drop-shadow-sm border-gray-400 hover:border-gray-500 focus-visible:ring-2 ring-offset-2 ring-yellow-400 data-state-checked:ring-blue-600 shrink-0',
          invalid && '!border-red-600 !hover:border-red-700',
          disabled && '!cursor-not-allowed !bg-gray-300 !border-gray-300'
        )}
        disabled={disabled}
      >
        <RadixCheckbox.Indicator
          {...options?.indicator}
          className={clsx(
            (internalCheckedState === true ||
              internalCheckedState === 'indeterminate') &&
              'absolute top-[-1px] right-[-1px] bottom-[-1px] left-[-1px] bg-blue-600 border border-blue-600 rounded',
            invalid && '!border-red-600 !hover:border-red-700',
            invalid && !disabled && '!bg-red-600',
            disabled && '!cursor-not-allowed !bg-gray-300 !border-gray-300'
          )}
        >
          {internalCheckedState === 'indeterminate' ? (
            <FaMinus
              className={clsx(
                'text-white absolute inset-px w-3 h-3 flex items-center'
              )}
              size="8px"
            />
          ) : internalCheckedState === true ? (
            <FaCheck
              className={clsx(
                'text-white absolute inset-px w-3 h-3 flex items-center'
              )}
              size="8px"
            />
          ) : null}
        </RadixCheckbox.Indicator>
      </RadixCheckbox.Root>
      {children ? (
        <label
          className={clsx(
            'font-normal m-0', // compensate boostrap leaking style
            'text-gray-900 pl-xs cursor-pointer',
            disabled && '!cursor-not-allowed'
          )}
          htmlFor={componentId}
        >
          {children}
        </label>
      ) : null}
    </div>
  );
};
