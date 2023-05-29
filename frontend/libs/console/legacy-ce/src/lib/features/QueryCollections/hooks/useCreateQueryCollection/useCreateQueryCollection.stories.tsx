import React from 'react';
import { handlers } from '../../../../mocks/metadata.mock';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../storybook/decorators/redux-decorator';
import ReactJson from 'react-json-view';
import { StoryObj, Meta } from '@storybook/react';
import { Button } from '../../../../new-components/Button';

import { useCreateQueryCollection } from './useCreateQueryCollection';
import { useQueryCollections } from '../useQueryCollections';

const UseQueryCollections: React.FC<{ name: string }> = ({ name }) => {
  const { data: queryCollections } = useQueryCollections();
  const { createQueryCollection, isSuccess, isLoading, error } =
    useCreateQueryCollection();

  return (
    <div>
      <ReactJson
        name="Data"
        src={{
          query_collections: queryCollections?.map(
            ({ name: collectionName }) => collectionName
          ),
        }}
      />
      <ReactJson
        name="Hook State"
        src={{
          isSuccess,
          isLoading,
          error: error?.message,
        }}
      />
      <Button onClick={() => createQueryCollection(name)}>
        Create Collection
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
  title: 'hooks/Query Collections/useCreateQueryCollection',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
  argTypes: {
    collectionName: {
      defaultValue: 'test',
      description: 'The name of the query collection to create',
      control: {
        type: 'text',
      },
    },
  },
} as Meta;
