import { useMetadataVersion } from '../../../../MetadataAPI';
import { Button } from '../../../../../new-components/Button';
import HookStatusWrapperWithMetadataVersion from '../../../../../storybook/HookStatusWrapperWithMetadataVersion';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../../storybook/decorators/redux-decorator';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';
import { useAddRemoteSchemaRelationship } from '..';

function AddRemoteSchemaRelationshipComponent({
  rel,
}: {
  rel: Record<string, unknown>;
}) {
  const mutation = useAddRemoteSchemaRelationship();
  const { data: version, isSuccess } = useMetadataVersion();
  return (
    <div>
      <Button onClick={() => mutation.mutate(rel)}>
        Add Remote Schema Relationship Mutation (Not compatible with CLI)
      </Button>
      <p className="mb-md text-muted mb-md pt-5">
        Adds a new remote schema relationship . The hook, on success,
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

export const AddRemoteSchemaRelationship: StoryObj<
  typeof AddRemoteSchemaRelationshipComponent
> = {
  render: args => {
    return <AddRemoteSchemaRelationshipComponent {...args} />;
  },

  args: {
    rel: {
      source: 'default',
      table: 'person',
      name: 'name_of_the_remote_relationship',
      definition: {
        to_remote_schema: {
          remote_schema: 'name_of_the_remote_schema',
          lhs_fields: ['id'],
          remote_field: {
            countries: {
              arguments: {
                filter: {
                  code: {
                    eq: '$id',
                  },
                },
              },
            },
          },
        },
      },
    },
  },
};

export default {
  title: 'hooks/Remote Schema Relationships/Create',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof AddRemoteSchemaRelationshipComponent>;
