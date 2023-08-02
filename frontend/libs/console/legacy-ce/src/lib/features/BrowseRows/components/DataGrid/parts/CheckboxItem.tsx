import clsx from 'clsx';
import React, { useEffect, useState } from 'react';
import { FaCheck, FaMinus } from 'react-icons/fa';
import * as Checkbox from '@radix-ui/react-checkbox';

type CheckboxProps = {
  isChecked?: boolean | 'indeterminate';
  isIndeterminate?: boolean;
  onChange: (event: unknown) => void;
  disabled?: boolean;
};

export const CheckboxItem: React.VFC<CheckboxProps> = ({
  isChecked = 'indeterminate',
  isIndeterminate = false,
  onChange,
  disabled = false,
}) => {
  const [checked, setChecked] = useState<boolean | 'indeterminate'>(isChecked);

  useEffect(() => {
    setChecked(isChecked);
  }, [isChecked]);

  useEffect(() => {
    if (isIndeterminate) {
      setChecked('indeterminate');
    }
  }, [isIndeterminate]);

  const twCheckboxStyle = clsx(
    'flex w-4 h-4 text-sm items-center justify-center rounded border shadow-sm border-gray-400 hover:border-gray-500 focus-visible:ring-2 focus-visible:ring-offset-2 focus-visible:ring-yellow-400',
    (checked === true || checked === 'indeterminate') &&
      'bg-blue-600 border-blue-600',
    disabled === true
      ? 'bg-gray-200 border-gray-400 cursor-not-allowed'
      : 'cursor-pointer'
  );

  return (
    <Checkbox.Root
      className={twCheckboxStyle}
      checked={checked}
      onClick={onChange}
      tabIndex={0}
      disabled={disabled}
    >
      <Checkbox.Indicator className="w- flex items-center justify-center">
        {checked === 'indeterminate' && <FaMinus size="8px" fill="white" />}
        {checked === true && <FaCheck size="8px" fill="white" />}
      </Checkbox.Indicator>
    </Checkbox.Root>
  );
};
