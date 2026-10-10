import React from 'react';
import { FaAngleRight } from 'react-icons/fa';
import clsx from 'clsx';
import { Flex } from '@radix-ui/themes';
import { Analytics } from '@hasura/shared/analytics';

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
  props: IconCardGroupProps<T>,
) => {
  const { value, items, disabled = false, onChange } = props;

  return (
    <div className="grid gap-sm grid-rows-auto w-full">
      {items.map((item) => {
        const { value: iValue, title, body } = item;
        return (
          <Analytics
            key={iValue}
            name={`hasura-familiarity-survey-${title}-option`}
          >
            <div
              className={clsx(
                'bg-white shadow-sm rounded p-4 border border-gray-300 flex',
                disabled ? 'cursor-not-allowed' : 'cursor-pointer',
                value === iValue && 'border-yellow-400',
              )}
              onClick={() => !disabled && onChange(iValue)}
            >
              <Flex align="center">{item.icon}</Flex>
              <div className="w-9/12 ml-4">
                <div
                  className={clsx(
                    'mt-0.5',
                    disabled ? 'cursor-not-allowed' : 'cursor-pointer',
                  )}
                >
                  {body}
                </div>
              </div>
              <Flex align="center" className="ml-auto">
                <FaAngleRight className="text-gray-500" />
              </Flex>
            </div>
          </Analytics>
        );
      })}
      <br />
    </div>
  );
};
