import React from 'react';
import { ReactQueryDecorator, handlers } from '@hasura/shared/testing';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { Button, JsonCodeBlock } from '@hasura/shared/ui';

import { useRemoveFromAllowList } from './useRemoveFromAllowList';

const UseQueryCollections: React.FC<{ name: string }> = ({ name }) => {
  const { removeFromAllowList, isSuccess, isLoading, error } =
    useRemoveFromAllowList();

  return (
    <div>
      <JsonCodeBlock
        value={{
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
  decorators: [ReactQueryDecorator()],
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
