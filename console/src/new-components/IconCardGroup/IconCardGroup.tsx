import clsx from 'clsx';
import React from 'react';
import { FaAngleRight } from 'react-icons/fa';

interface IconCardGroupItem<T> {
  value: T;
  icon: React.ReactNode;
  title: string;
  body: string | React.ReactNode;
}

interface IconCardGroupProps<T> {
  items: Array<IconCardGroupItem<T>>;
  onChange: (option: T) => void;
  disabled?: boolean;
  value?: T;
}

export const IconCardGroup = <T extends string = string>(
  props: IconCardGroupProps<T>
) => {
  const { value, items, disabled = false, onChange } = props;

  return (
    <div className="grid gap-sm grid-rows-auto w-full">
      {items.map(item => {
        const { value: iValue, title, body } = item;
        return (
          <div
            className={clsx(
              'bg-white shadow-sm rounded p-md border border-gray-300 flex',
              disabled ? 'cursor-not-allowed' : 'cursor-pointer',
              value === iValue && 'border-yellow-400'
            )}
            key={iValue}
            onClick={() => !disabled && onChange(iValue)}
          >
            <div className="mt-2">{item.icon}</div>
            <div className="w-9/12 ml-md">
              <label
                htmlFor={`card-select-${iValue}`}
                className={clsx(
                  'mb-sm font-semibold mt-0.5',
                  disabled ? 'cursor-not-allowed' : 'cursor-pointer'
                )}
              >
                {title}
              </label>
              <p className="text-muted">{body}</p>
            </div>
            <div className="mt-2 ml-auto">
              <FaAngleRight className="text-gray-500" />
            </div>
          </div>
        );
      })}
      <br />
    </div>
  );
};
