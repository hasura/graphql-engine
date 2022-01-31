import { useMetadataVersion } from '@/features/MetadataAPI';
import { Button } from '@/new-components/Button';
import HookStatusWrapperWithMetadataVersion from '@/storybook/HookStatusWrapperWithMetadataVersion';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { useUpdateRemoteDatabaseRelationship } from '..';

function UpdateRemoteDatabaseRelationshipComponent({
  rel,
}: {
  rel: Record<string, unknown>;
}) {
  const mutation = useUpdateRemoteDatabaseRelationship();
  const { data: version, isSuccess } = useMetadataVersion();
  return (
    <div>
      <Button onClick={() => mutation.mutate(rel)}>
        Update Remote Database Relationship Mutation (Not compatible with CLI)
      </Button>
      <p className="mb-md text-muted mb-md pt-5">
        Updates an existing remote relationship from the metadata using the
        name, source & table as the input parameters. The hook, on success,
        invalidates the existing metadata and refetches it again.
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

export const UpdateRemoteDatabaseRelationship: ComponentStory<
  typeof UpdateRemoteDatabaseRelationshipComponent
> = args => {
  return <UpdateRemoteDatabaseRelationshipComponent {...args} />;
};

UpdateRemoteDatabaseRelationship.args = {
  rel: {
    source: 'default',
    table: 'test',
    name: 'name_of_the_remote_relationship',
    definition: {
      to_source: {
        source: 'chinook',
        table: 'Artist',
        relationship_type: 'object',
        field_mapping: {
          id: 'ArtistId',
        },
      },
    },
  },
};

UpdateRemoteDatabaseRelationship.parameters = {
  // Disable storybook for Remote Relationship stories
  chromatic: { disableSnapshot: true },
};

export default {
  title: 'hooks/Remote Database Relationships/Update',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof UpdateRemoteDatabaseRelationshipComponent>;
