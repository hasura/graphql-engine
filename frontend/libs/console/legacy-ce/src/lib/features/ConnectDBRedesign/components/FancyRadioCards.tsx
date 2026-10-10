import { Flex, RadioCards } from '@radix-ui/themes';
import React from 'react';

export const FancyRadioCards: React.FC<{
  value: string;
  items: {
    value: string;
    content: React.ReactNode | string;
  }[];
  onChange: (value: string) => void;
}> = ({ value, items, onChange }) => {
  return (
    <div className="mb-4">
      <RadioCards.Root
        value={value}
        aria-label="Radio cards"
        onValueChange={onChange}
        columns={{
          initial: '2',
          sm: '4',
        }}
      >
        {items.map((item, i) => {
          return (
            <RadioCards.Item
              key={item.value}
              value={item.value}
              data-testid={`fancy-radio-${item.value}`}
              id={`radio-item-${item.value}`}
            >
              <Flex align="center" justify="center" className="h-[88px]">
                {item.content}
              </Flex>
            </RadioCards.Item>
          );
        })}
      </RadioCards.Root>
    </div>
  );
};
