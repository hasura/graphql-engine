import React from 'react';
import { action } from '@storybook/addon-actions';
import { StoryObj, StoryFn, Meta } from '@storybook/react';
import { userEvent, within } from '@storybook/testing-library';
import { FaStar, FaHeart, FaUser, FaBookmark } from 'react-icons/fa';
import { waitFor } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { IconCardGroup } from './IconCardGroup';

export default {
  title: 'components/IconCardGroup',
  component: IconCardGroup,
  argTypes: {
    onCardClick: { action: true },
  },
} as Meta<typeof IconCardGroup>;

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

export const CardsWithValue: StoryFn = () => {
  return (
    <IconCardGroup<Value> onChange={action('select')} items={data} value="1" />
  );
};

export const CardsWithoutValue: StoryFn = () => {
  return <IconCardGroup<Value> onChange={action('select')} items={data} />;
};

export const Playground: StoryFn = () => {
  const [selected, setSelected] = React.useState<string | undefined>(undefined);

  return (
    <IconCardGroup
      onChange={value => setSelected(value)}
      items={data}
      value={selected}
    />
  );
};

export const PlaygroundWithTest: StoryObj = {
  render: args => {
    return (
      <IconCardGroup onChange={value => args.onCardClick(value)} items={data} />
    );
  },

  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);

    // Click on the first card, and expect the onChange prop to be called with `1`
    await userEvent.click(canvas.getByText('Description of card-1'));

    await waitFor(() => expect(args.onCardClick).toHaveBeenCalledTimes(1));

    expect(args.onCardClick).toBeCalledWith('1');

    // Click on the fourth card, and expect the onChange prop to be called with `4`
    await userEvent.click(canvas.getByText('Description of card-4'));

    await waitFor(() => expect(args.onCardClick).toHaveBeenCalledTimes(2));

    expect(args.onCardClick).toBeCalledWith('4');
  },
};
