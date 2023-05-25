import { action } from '@storybook/addon-actions';
import { StoryObj, StoryFn, Meta } from '@storybook/react';
import { userEvent, within } from '@storybook/testing-library';
import React from 'react';
import { CardRadioGroup } from './CardRadioGroup';

export default {
  title: 'components/CardRadioGroup',
  component: CardRadioGroup,
} as Meta<typeof CardRadioGroup>;

type Value = '1' | '2' | '3' | '4' | '5' | '6' | '7';

const data: { value: Value; title: string; body: string }[] = [
  { value: '1', title: 'Radio-1', body: 'Description of radio-1' },
  { value: '2', title: 'Radio-2', body: 'Description of radio-2' },
  { value: '3', title: 'Radio-3', body: 'Description of radio-3' },
];

export const WithTwoCards: StoryFn = () => {
  return (
    <CardRadioGroup<'1' | '2'>
      onChange={action('select')}
      items={[
        { value: '1', title: 'Radio-1', body: 'Description of radio-1' },
        { value: '2', title: 'Radio-2', body: 'Description of radio-2' },
      ]}
    />
  );
};

export const WithThreeCardWithoutValue: StoryFn = () => {
  return <CardRadioGroup<Value> onChange={action('select')} items={data} />;
};

export const WithThreeCardVerticalWithoutValue: StoryFn = () => {
  return (
    <CardRadioGroup<Value>
      onChange={action('select')}
      items={data}
      orientation="vertical"
    />
  );
};

export const WithThreeCardWithValue: StoryFn = () => {
  return (
    <CardRadioGroup<Value> onChange={action('select')} items={data} value="1" />
  );
};

export const WithThreeCardVerticalWithValue: StoryFn = () => {
  return (
    <CardRadioGroup<Value>
      onChange={action('select')}
      items={data}
      value="1"
      orientation="vertical"
    />
  );
};

export const WithSevenCardWithValue: StoryFn = () => {
  return (
    <CardRadioGroup<Value>
      onChange={action('select')}
      items={[
        { value: '1', title: 'Radio-1', body: 'Description of radio-1' },
        { value: '2', title: 'Radio-2', body: 'Description of radio-2' },
        { value: '3', title: 'Radio-3', body: 'Description of radio-3' },
        { value: '4', title: 'Radio-4', body: 'Description of radio-4' },
        { value: '5', title: 'Radio-5', body: 'Description of radio-5' },
        { value: '6', title: 'Radio-6', body: 'Description of radio-6' },
        { value: '7', title: 'Radio-7', body: 'Description of radio-7' },
      ]}
      value="1"
    />
  );
};

export const Playground: StoryFn = () => {
  const [selected, setSelected] = React.useState<string | undefined>(undefined);

  return (
    <CardRadioGroup
      onChange={value => setSelected(value)}
      items={data}
      value={selected}
    />
  );
};

export const PlaygroundWithTest: StoryObj = {
  render: () => {
    const [selected, setSelected] = React.useState<string | undefined>(
      undefined
    );

    return (
      <CardRadioGroup
        onChange={value => setSelected(value)}
        items={data}
        value={selected}
      />
    );
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const submitButtonRadio1 = await canvas.findByText('Radio-1');
    userEvent.click(submitButtonRadio1);

    const submitButtonRadio2 = await canvas.findByText('Radio-2');
    userEvent.click(submitButtonRadio2);

    const submitButtonRadio3 = await canvas.findByText('Radio-3');
    userEvent.click(submitButtonRadio3);
  },
};
