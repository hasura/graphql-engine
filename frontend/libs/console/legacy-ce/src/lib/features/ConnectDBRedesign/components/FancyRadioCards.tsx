import * as RadioGroup from '@radix-ui/react-radio-group';
import clsx from 'clsx';
import React, { VFC } from 'react';

const twRadioStyles = {
  root: `grid grid-cols-4 gap-3`,
  itemContainer: {
    default: `flex items-center border bg-white shadow-sm rounded border-gray-300 cursor-pointer relative flex-[0_0_160px] h-[88px]`,
    active: `ring-2 ring-blue-300 border-blue-400`,
    disabled: ` cursor-not-allowed bg-gray-200`,
  },
  radioButton: `bg-white w-[20px] h-[20px] rounded-full shadow-eq shadow-blue-900 hover:bg-blue-100 flex-[2] absolute top-0 left-0 m-3`,
  indicator: `flex items-center justify-center w-full h-full relative after:content[''] after:block after:w-[10px] after:h-[10px] after:rounded-[50%] after:bg-blue-600`,
  label: `text-base whitespace-nowrap cursor-pointer flex-[1] h-full w-full flex justify-center items-center`,
};

export const FancyRadioCards: VFC<{
  value: string;
  items: {
    value: string;
    content: React.ReactNode | string;
  }[];
  onChange: (value: string) => void;
}> = ({ value, items, onChange }) => {
  return (
    <RadioGroup.Root
      className={twRadioStyles.root}
      value={value}
      aria-label="Radio cards"
      onValueChange={onChange}
    >
      {items.map((item, i) => {
        return (
          <div
            key={item.value}
            className={clsx(
              twRadioStyles.itemContainer.default,
              value === item.value && twRadioStyles.itemContainer.active
            )}
          >
            <RadioGroup.Item
              className={twRadioStyles.radioButton}
              value={item.value}
              data-testid={`fancy-radio-${item.value}`}
              id={`radio-item-${item.value}`}
            >
              <RadioGroup.Indicator className={twRadioStyles.indicator} />
            </RadioGroup.Item>
            <label
              className={twRadioStyles.label}
              data-testid={`fancy-label-${item.value}`}
              htmlFor={`radio-item-${item.value}`}
            >
              {item.content}
            </label>
          </div>
        );
      })}
    </RadioGroup.Root>
  );
};
