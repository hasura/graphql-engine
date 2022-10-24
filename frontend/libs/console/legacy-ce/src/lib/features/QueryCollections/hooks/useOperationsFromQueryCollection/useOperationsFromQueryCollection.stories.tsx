import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import ReactJson from 'react-json-view';
import { Meta, Story } from '@storybook/react';
import React from 'react';

import { useOperationsFromQueryCollection } from './useOperationsFromQueryCollection';
import { handlers } from './mocks/handlers.mock';

function UseOperationsFromQueryCollection() {
  const operations = useOperationsFromQueryCollection('allowed-queries');

  const error = operations.error;

  return (
    <div>
      {operations.isSuccess ? (
        <ReactJson collapsed src={operations.data} />
      ) : (
        'no response'
      )}

      {error ? <ReactJson src={error} /> : null}
    </div>
  );
}

export const Primary: Story = () => {
  return <UseOperationsFromQueryCollection />;
};

export default {
  title: 'hooks/useOperationsFromQueryCollection',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers(),
  },
} as Meta;
