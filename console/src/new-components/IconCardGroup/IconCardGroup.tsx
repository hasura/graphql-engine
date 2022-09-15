import clsx from 'clsx';
import React from 'react';
import { FaAngleRight } from 'react-icons/fa';

export interface IconCardGroupItem<T> {
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
            data-trackid={`hasura-familiarity-survey-${title}-option`}
          >
            <div className="flex items-center">{item.icon}</div>
            <div className="w-9/12 ml-md">
              <div
                className={clsx(
                  'mt-0.5',
                  disabled ? 'cursor-not-allowed' : 'cursor-pointer'
                )}
              >
                {body}
              </div>
            </div>
            <div className="flex items-center ml-auto">
              <FaAngleRight className="text-gray-500" />
            </div>
          </div>
        );
      })}
      <br />
    </div>
  );
};
