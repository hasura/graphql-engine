import { useMetadataVersion } from '../../../../MetadataAPI';
import { Button } from '../../../../../new-components/Button';
import HookStatusWrapperWithMetadataVersion from '../../../../../storybook/HookStatusWrapperWithMetadataVersion';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../../storybook/decorators/redux-decorator';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';
import { useUpdateRemoteSchemaRelationship } from '..';

function UpdateRemoteSchemaRelationshipComponent({
  rel,
}: {
  rel: Record<string, unknown>;
}) {
  const mutation = useUpdateRemoteSchemaRelationship();
  const { data: version, isSuccess } = useMetadataVersion();
  return (
    <div>
      <Button onClick={() => mutation.mutate(rel)}>
        Update Remote Schema Relationship Mutation (Not compatible with CLI)
      </Button>
      <p className="mb-md text-muted mb-md pt-5">
        Updates an existing remote schema relationship from the metadata using
        the name, source & table as the input parameters. The hook, on success,
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

export const UpdateRemoteSchemaRelationship: StoryObj<
  typeof UpdateRemoteSchemaRelationshipComponent
> = {
  render: args => {
    return <UpdateRemoteSchemaRelationshipComponent {...args} />;
  },

  args: {
    rel: {
      source: 'default',
      table: 'person',
      name: 'name_of_the_remote_relationship',
      definition: {
        to_remote_schema: {
          remote_schema: 'name_of_the_remote_schema',
          lhs_fields: ['test_id'],
          remote_field: {
            countries: {
              arguments: {
                filter: {
                  code: {
                    eq: '$test_id',
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
  title: 'hooks/Remote Schema Relationships/Update',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof UpdateRemoteSchemaRelationshipComponent>;
