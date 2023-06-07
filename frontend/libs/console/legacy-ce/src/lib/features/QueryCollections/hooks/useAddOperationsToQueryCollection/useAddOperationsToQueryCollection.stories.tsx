import React from 'react';
import { handlers } from '../../../../mocks/metadata.mock';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../storybook/decorators/redux-decorator';
import ReactJson from 'react-json-view';
import { Button } from '../../../../new-components/Button';
import { StoryFn, Meta } from '@storybook/react';

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
              query: `query MyQuery { user { email name } }`,
              name: 'MyQuery7',
            },
          ])
        }
      >
        Add MyQuery to Query Collection
      </Button>
    </div>
  );
};

export const Primary: StoryFn = () => {
  return <UseAddOperationsToQueryCollection />;
};

export default {
  title: 'hooks/Query Collections/useAddOperationsToQueryCollection',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
} as Meta;
