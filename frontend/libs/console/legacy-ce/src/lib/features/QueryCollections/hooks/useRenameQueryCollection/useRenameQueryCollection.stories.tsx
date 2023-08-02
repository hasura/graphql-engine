import React from 'react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../storybook/decorators/redux-decorator';
import ReactJson from 'react-json-view';
import { StoryObj, Meta } from '@storybook/react';
import { Button } from '../../../../new-components/Button';

import { handlers } from '../../../../mocks/metadata.mock';
import { useRenameQueryCollection } from './useRenameQueryCollection';
import { useQueryCollections } from '../useQueryCollections';

const UseQueryCollections: React.FC<{ name: string; newName: string }> = ({
  name,
  newName,
}) => {
  const { data: queryCollections } = useQueryCollections();
  const { renameQueryCollection, isSuccess, isLoading, error } =
    useRenameQueryCollection();

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
      <Button onClick={() => renameQueryCollection(name, newName)}>
        Rename Collection
      </Button>
    </div>
  );
};

export const Primary: StoryObj = {
  render: ({ name, newName }) => {
    return <UseQueryCollections name={name} newName={newName} />;
  },
};

export default {
  title: 'hooks/Query Collections/useRenameQueryCollection',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
  argTypes: {
    name: {
      defaultValue: 'allowed-queries',
      description: 'The name of the query collection to rename',
      control: {
        type: 'text',
      },
    },
    newName: {
      defaultValue: 'queries_allowed',
      description: 'The new name of the query collection',
      control: {
        type: 'text',
      },
    },
  },
} as Meta;
