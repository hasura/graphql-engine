import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import ReactJson from 'react-json-view';
import { Button } from '@/new-components/Button';
import { Meta, Story } from '@storybook/react';

import { handlers } from './mocks/handlers.mock';
import { useRemoveOperationsFromQueryCollection } from '.';

const UseRemoveOperationsFromQueryCollection: React.FC = () => {
  const { removeOperationsFromQueryCollection, isSuccess, isLoading, error } =
    useRemoveOperationsFromQueryCollection();

  return (
    <div>
      <ReactJson
        name="Hook State"
        src={{
          isSuccess,
          isLoading,
          error: error?.message,
        }}
      />
      <Button
        onClick={() =>
          removeOperationsFromQueryCollection('allowed-queries', [
            {
              query: `query MyQuery { user { email name}}`,
              name: 'MyQuery',
            },
          ])
        }
      >
        Add MyQuery to Query Collection
      </Button>
    </div>
  );
};

export const Primary: Story = () => {
  return <UseRemoveOperationsFromQueryCollection />;
};

export default {
  title: 'hooks/Query Collections/useRemoveOperationsFromQueryCollection',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers(1000),
  },
} as Meta;
