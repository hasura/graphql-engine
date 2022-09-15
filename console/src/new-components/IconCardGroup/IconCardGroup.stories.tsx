import React from 'react';
import { action } from '@storybook/addon-actions';
import { ComponentMeta, Story } from '@storybook/react';
import { FaStar, FaHeart, FaUser, FaBookmark } from 'react-icons/fa';
import { IconCardGroup } from './IconCardGroup';

export default {
  title: 'components/IconCardGroup',
  component: IconCardGroup,
  argTypes: {
    onCardClick: { action: true },
  },
} as ComponentMeta<typeof IconCardGroup>;

type Value = '1' | '2' | '3' | '4';

const data: {
  value: Value;
  icon: React.ReactNode;
  title: string;
  body: string;
}[] = [
  {
    value: '1',
    icon: <FaStar className="text-yellow-500 text-xl" />,
    title: 'Card-1',
    body: 'Description of card-1',
  },
  {
    value: '2',
    icon: <FaHeart className="text-yellow-500 text-xl" />,
    title: 'Card-2',
    body: 'Description of card-2',
  },
  {
    value: '3',
    icon: <FaUser className="text-yellow-500 text-xl" />,
    title: 'Card-3',
    body: 'Description of card-3',
  },
  {
    value: '4',
    icon: <FaBookmark className="text-yellow-500 text-xl" />,
    title: 'Card-4',
    body: 'Description of card-4',
  },
];

export const CardsWithValue: Story = () => {
  return (
    <IconCardGroup<Value> onChange={action('select')} items={data} value="1" />
  );
};

export const CardsWithoutValue: Story = () => {
  return <IconCardGroup<Value> onChange={action('select')} items={data} />;
};

export const Playground: Story = () => {
  const [selected, setSelected] = React.useState<string | undefined>(undefined);

  return (
    <IconCardGroup
      onChange={value => setSelected(value)}
      items={data}
      value={selected}
    />
  );
};
