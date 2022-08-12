import { action } from '@storybook/addon-actions';
import { ComponentMeta, Story } from '@storybook/react';
import { userEvent, within } from '@storybook/testing-library';
import React from 'react';
import { CardRadioGroup } from './CardRadioGroup';

export default {
  title: 'components/CardRadioGroup',
  component: CardRadioGroup,
} as ComponentMeta<typeof CardRadioGroup>;

type Value = '1' | '2' | '3';

const data: { value: Value; title: string; body: string }[] = [
  { value: '1', title: 'Radio-1', body: 'Description of radio-1' },
  { value: '2', title: 'Radio-2', body: 'Description of radio-2' },
  { value: '3', title: 'Radio-3', body: 'Description of radio-3' },
];

export const WithTwoCards: Story = () => {
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

export const WithThreeCardWithoutValue: Story = () => {
  return <CardRadioGroup<Value> onChange={action('select')} items={data} />;
};

export const WithThreeCardWithValue: Story = () => {
  return (
    <CardRadioGroup<Value> onChange={action('select')} items={data} value="1" />
  );
};

export const Playground: Story = () => {
  const [selected, setSelected] = React.useState<string | undefined>(undefined);

  return (
    <CardRadioGroup
      onChange={value => setSelected(value)}
      items={data}
      value={selected}
    />
  );
};

export const PlaygroundWithTest: Story = () => {
  const [selected, setSelected] = React.useState<string | undefined>(undefined);

  return (
    <CardRadioGroup
      onChange={value => setSelected(value)}
      items={data}
      value={selected}
    />
  );
};

PlaygroundWithTest.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  const submitButtonRadio1 = await canvas.findByText('Radio-1');
  userEvent.click(submitButtonRadio1);

  const submitButtonRadio2 = await canvas.findByText('Radio-2');
  userEvent.click(submitButtonRadio2);

  const submitButtonRadio3 = await canvas.findByText('Radio-3');
  userEvent.click(submitButtonRadio3);
};
