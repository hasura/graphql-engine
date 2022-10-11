import { useMetadataVersion } from '@/features/MetadataAPI';
import { Button } from '@/new-components/Button';
import HookStatusWrapperWithMetadataVersion from '@/storybook/HookStatusWrapperWithMetadataVersion';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { useDropRemoteDatabaseRelationship } from '..';

function DeleteRemoteDatabaseRelationshipComponent({
  rel,
}: {
  rel: Record<string, unknown>;
}) {
  const mutation = useDropRemoteDatabaseRelationship();
  const { data: version, isSuccess } = useMetadataVersion();
  return (
    <div>
      <Button onClick={() => mutation.mutate(rel)}>
        Delete Remote Database Relationship Mutation (Not compatible with CLI)
      </Button>
      <p className="mb-md text-muted mb-md pt-5">
        Delets a remote relationship from the metadata using the name, source &
        table as the input parameters. The hook, on success, invalidates the
        existing metadata and refetches it again.
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

export const DeleteRemoteDatabaseRelationship: ComponentStory<
  typeof DeleteRemoteDatabaseRelationshipComponent
> = args => {
  return <DeleteRemoteDatabaseRelationshipComponent {...args} />;
};

DeleteRemoteDatabaseRelationship.args = {
  rel: {
    source: 'default',
    table: 'test',
    name: 'name_of_the_remote_relationship',
  },
};

DeleteRemoteDatabaseRelationship.parameters = {
  // Disable storybook for Remote Relationship stories
  chromatic: { disableSnapshot: true },
};

export default {
  title: 'hooks/Remote Database Relationships/Delete',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof DeleteRemoteDatabaseRelationshipComponent>;
