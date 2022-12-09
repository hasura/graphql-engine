import { useMetadataVersion } from '@/features/MetadataAPI';
import { Button } from '@/new-components/Button';
import HookStatusWrapperWithMetadataVersion from '@/storybook/HookStatusWrapperWithMetadataVersion';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { useAddRemoteDatabaseRelationship } from '..';

function AddRemoteDatabaseRelationshipComponent({
  rel,
}: {
  rel: Record<string, unknown>;
}) {
  const mutation = useAddRemoteDatabaseRelationship();
  const { data: version, isSuccess } = useMetadataVersion();
  return (
    <div>
      <Button onClick={() => mutation.mutate(rel)}>
        Add Remote Database Relationship Mutation (Not compatible with CLI)
      </Button>
      <p className="mb-md text-muted mb-md pt-5">
        Adds a new relationship from the source database table to the target
        database table. The hook, on success, invalidates the existing metadata
        and refetches it again.
      </p>
      <HookStatusWrapperWithMetadataVersion
        status={{
          isSuccess: mutation.isSuccess,
          isError: mutation.isError,
          isLoading: mutation.isLoading,
          isIdle: mutation.isIdle,
        }}
        metadata={{ version, isSuccess }}
      />
    </div>
  );
}

export const AddRemoteDatabaseRelationship: ComponentStory<
  typeof AddRemoteDatabaseRelationshipComponent
> = args => {
  return <AddRemoteDatabaseRelationshipComponent {...args} />;
};

AddRemoteDatabaseRelationship.args = {
  rel: {
    source: 'default',
    table: 'test',
    name: 'name_of_the_remote_relationship',
    definition: {
      to_source: {
        source: 'chinook',
        table: 'Album',
        relationship_type: 'object',
        field_mapping: {
          id: 'AlbumId',
        },
      },
    },
  },
};

AddRemoteDatabaseRelationship.parameters = {
  // Disable storybook for Remote Relationship stories
  chromatic: { disableSnapshot: true },
};

export default {
  title: 'hooks/Remote Database Relationships/Create',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof AddRemoteDatabaseRelationshipComponent>;
