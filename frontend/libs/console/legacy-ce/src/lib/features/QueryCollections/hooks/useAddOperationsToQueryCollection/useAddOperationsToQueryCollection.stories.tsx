import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import ReactJson from 'react-json-view';
import { Button } from '@/new-components/Button';
import { Meta, Story } from '@storybook/react';

import { handlers } from './mocks/handlers.mock';
import { useAddOperationsToQueryCollection } from '.';

const UseAddOperationsToQueryCollection: React.FC = () => {
  const { addOperationToQueryCollection, isSuccess, isLoading, error } =
    useAddOperationsToQueryCollection();

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
          addOperationToQueryCollection('allowed-queries', [
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
  return <UseAddOperationsToQueryCollection />;
};

export default {
  title: 'hooks/Query Collections/useAddOperationsToQueryCollection',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers(1000),
  },
} as Meta;
