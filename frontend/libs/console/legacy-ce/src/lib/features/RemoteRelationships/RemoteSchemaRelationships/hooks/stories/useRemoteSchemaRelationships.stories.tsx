import { useRemoteSchemaRelationships } from '@/features/MetadataAPI';
import { QualifiedTable } from '@/metadata/types';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';

function GetRemoteSchemaRelationshipsComponent({
  database,
  qualifiedTable,
}: {
  database: string;
  qualifiedTable: QualifiedTable;
}) {
  const query = useRemoteSchemaRelationships(database, qualifiedTable);
  return (
    <div>
      <b>Get Remote Schema Relationship Query</b>
      <p className="mb-md text-muted mb-md pt-5">
        Gets the list of Remote Schema Relationships available for a given
        database & qualified table.
      </p>
      {JSON.stringify(query.data)}
    </div>
  );
}

export const GetRemoteSchemaRelationships: ComponentStory<
  typeof GetRemoteSchemaRelationshipsComponent
> = args => {
  return <GetRemoteSchemaRelationshipsComponent {...args} />;
};

GetRemoteSchemaRelationships.args = {
  database: 'default',
  qualifiedTable: {
    schema: 'public',
    name: 'person',
  },
};

GetRemoteSchemaRelationships.parameters = {
  // Disable storybook for Remote Relationship stories
  chromatic: { disableSnapshot: true },
};

export default {
  title: 'hooks/Remote Schema Relationships/Fetch',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof GetRemoteSchemaRelationshipsComponent>;
