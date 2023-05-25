import React, { useState } from 'react';
import { userEvent, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { StoryObj, StoryFn, Meta } from '@storybook/react';
import { TableTabView } from './TableTabView';

export default {
  component: TableTabView,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof TableTabView>;

const Component = () => {
  const [tabs, setTabs] = useState<{ value: string; label: string }[]>([
    {
      value: 'Tab-1-value',
      label: 'Tab-1',
    },
    {
      value: 'Tab-2-value',
      label: 'Tab-2',
    },
  ]);

  const [activeTab, setActiveTab] = useState('Tab-1-value');

  return (
    <TableTabView
      items={tabs.map(tab => ({
        value: tab.value,
        label: tab.label,
        content: <div data-testid="@display-value">{tab.value}</div>,
      }))}
      activeTab={activeTab}
      onTabClick={value => {
        setActiveTab(value);
      }}
      onTabClose={value =>
        setTabs(oldTabs => oldTabs.filter(tab => tab.value !== value))
      }
      onCloseAll={() => {}}
    />
  );
};

export const Primary: StoryFn<typeof TableTabView> = () => <Component />;

export const Testing: StoryObj<typeof TableTabView> = {
  render: () => <Component />,
  name: '🧪 Test - Basic user interaction',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const firstTab = await canvas.findByTestId('@tab-Tab-1');
    expect(firstTab).toBeVisible();

    const secondTab = await canvas.findByTestId('@tab-Tab-2');
    expect(secondTab).toBeVisible();

    expect(await canvas.findByTestId('@display-value')).toHaveTextContent(
      'Tab-1-value'
    );

    userEvent.click(secondTab);

    expect(await canvas.findByTestId('@display-value')).toHaveTextContent(
      'Tab-2-value'
    );
  },
};
