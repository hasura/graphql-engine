import React from 'react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../storybook/decorators/redux-decorator';
import ReactJson from 'react-json-view';
import { StoryObj, Meta } from '@storybook/react';
import { Button } from '../../../../new-components/Button';

import { handlers } from '../../../../mocks/metadata.mock';
import { useRemoveFromAllowList } from './useRemoveFromAllowList';

const UseQueryCollections: React.FC<{ name: string }> = ({ name }) => {
  const { removeFromAllowList, isSuccess, isLoading, error } =
    useRemoveFromAllowList();

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
      <Button onClick={() => removeFromAllowList(name)}>
        Remove Collection
      </Button>
    </div>
  );
};

export const Primary: StoryObj = {
  render: ({ collectionName }) => {
    return <UseQueryCollections name={collectionName} />;
  },
};

export default {
  title: 'hooks/Allow List/useRemoveFromAllowList',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
  argTypes: {
    collectionName: {
      defaultValue: 'allowed-queries',
      description:
        'The name of the query collection to remove from the allow list',
      control: {
        type: 'text',
      },
    },
  },
} as Meta;
