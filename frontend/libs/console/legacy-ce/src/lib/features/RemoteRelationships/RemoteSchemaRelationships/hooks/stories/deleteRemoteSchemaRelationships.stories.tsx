import { useMetadataVersion } from '../../../../MetadataAPI';
import { Button } from '../../../../../new-components/Button';
import HookStatusWrapperWithMetadataVersion from '../../../../../storybook/HookStatusWrapperWithMetadataVersion';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../../storybook/decorators/redux-decorator';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';
import { useDropRemoteSchemaRelationship } from '..';

function DeleteRemoteSchemaRelationshipComponent({
  rel,
}: {
  rel: Record<string, unknown>;
}) {
  const mutation = useDropRemoteSchemaRelationship();
  const { data: version, isSuccess } = useMetadataVersion();
  return (
    <div>
      <Button onClick={() => mutation.mutate(rel)}>
        Delete Remote Schema Relationship Mutation (Not compatible with CLI)
      </Button>
      <p className="mb-md text-muted mb-md pt-5">
        Delets a remote schema relationship from the metadata. The hook, on
        success, invalidates the existing metadata and refetches it again.
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

export const DeleteRemoteSchemaRelationship: StoryObj<
  typeof DeleteRemoteSchemaRelationshipComponent
> = {
  render: args => {
    return <DeleteRemoteSchemaRelationshipComponent {...args} />;
  },

  args: {
    rel: {
      source: 'default',
      table: 'person',
      name: 'name_of_the_remote_relationship',
    },
  },
};

export default {
  title: 'hooks/Remote Schema Relationships/Delete',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof DeleteRemoteSchemaRelationshipComponent>;
