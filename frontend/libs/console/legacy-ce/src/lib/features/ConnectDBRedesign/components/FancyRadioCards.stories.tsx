import { expect } from '@storybook/jest';
import { Meta, StoryFn } from '@storybook/react';
import { userEvent, within } from '@storybook/testing-library';
import React from 'react';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { handlers } from '../mocks/handlers.mock';

import { useDatabaseConnectDrivers } from '../hooks';
import { FancyRadioCards } from './FancyRadioCards';
import { Badge } from '../../../new-components/Badge';

export default {
  component: FancyRadioCards,
  parameters: {
    msw: handlers(),
  },
  decorators: [ReactQueryDecorator()],
} as Meta<typeof FancyRadioCards>;

const Template: StoryFn<typeof FancyRadioCards> = () => {
  // using this hook as it has a handy card data return so the story has an example of icons + text

  const { cardData } = useDatabaseConnectDrivers({
    showEnterpriseDrivers: true,
  });

  const [value, setValue] = React.useState('postgres');

  return (
    <div>
      <div className="mb-3">
        Selected Value:
        <Badge className="ml-2">
          <span data-testid="value">{value}</span>
        </Badge>
      </div>
      <FancyRadioCards value={value} items={cardData} onChange={setValue} />
    </div>
  );
};

export const Primary = {
  render: Template,

  play: async ({ canvasElement }) => {
    const c = within(canvasElement);

    // test click on label element
    await userEvent.click(await c.findByTestId('fancy-label-snowflake'));

    expect(c.getByTestId('value')).toHaveTextContent('snowflake');

    // test click on radio button
    await userEvent.click(await c.findByTestId('fancy-radio-mssql'));

    expect(c.getByTestId('value')).toHaveTextContent('mssql');
  },
};
