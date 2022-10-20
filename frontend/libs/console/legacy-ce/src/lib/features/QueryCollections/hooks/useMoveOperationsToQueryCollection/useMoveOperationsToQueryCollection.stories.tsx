import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import ReactJson from 'react-json-view';
import { Button } from '@/new-components/Button';
import { Meta, Story } from '@storybook/react';

import { handlers } from './mocks/handlers.mock';
import { useMoveOperationsToQueryCollection } from '.';

const UseMoveOperationsToQueryCollection: React.FC = () => {
  const { moveOperationToQueryCollection, isSuccess, isLoading, error } =
    useMoveOperationsToQueryCollection();

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
          moveOperationToQueryCollection('fromCollection', 'targetCollection', [
            {
              name: 'NewMyQuery',
              query: 'query NewMyQuery { user { email name}}',
            },
          ])
        }
      >
        Move MyQuery to targetCollection
      </Button>
    </div>
  );
};

export const Primary: Story = () => {
  return <UseMoveOperationsToQueryCollection />;
};

export default {
  title: 'hooks/Query Collections/useMoveOperationsToQueryCollection',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers(1000),
  },
} as Meta;
