import React from 'react';
import { ReactQueryDecorator, handlers } from '@hasura/shared/testing';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { Button, JsonCodeBlock } from '@hasura/shared/ui';

import { useAddToAllowList } from './useAddToAllowList';

const UseQueryCollections: React.FC<{ name: string }> = ({ name }) => {
  const { addToAllowList, isSuccess, isLoading, error } = useAddToAllowList();

  return (
    <div>
      <JsonCodeBlock
        value={{
          isSuccess,
          isLoading,
          error: error?.message,
        }}
      />
      <Button onClick={() => addToAllowList(name)}>Add Collection</Button>
    </div>
  );
};

export const Primary: StoryObj = {
  render: ({ collectionName }) => {
    return <UseQueryCollections name={collectionName} />;
  },
};

export default {
  title: 'hooks/Allow List/useAddToAllowList',
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
  argTypes: {
    collectionName: {
      defaultValue: 'new-queries',
      description: 'The name of the query collection to add to the allow list',
      control: {
        type: 'text',
      },
    },
  },
} as Meta;
