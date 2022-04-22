import { useRemoteDatabaseRelationships } from '@/features/MetadataAPI';
import { QualifiedTable } from '@/metadata/types';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';

function GetRemoteDatabaseRelationshipsComponent({
  database,
  qualifiedTable,
}: {
  database: string;
  qualifiedTable: QualifiedTable;
}) {
  const query = useRemoteDatabaseRelationships(database, qualifiedTable);
  return (
    <div>
      <b>Get Remote Database Relationship Query</b>
      <p className="mb-md text-muted mb-md pt-5">
        Gets the list of Remote Database Relationships available for a given
        database & qualified table.
      </p>
      {JSON.stringify(query.data)}
    </div>
  );
}

export const GetRemoteDatabaseRelationships: ComponentStory<
  typeof GetRemoteDatabaseRelationshipsComponent
> = args => {
  return <GetRemoteDatabaseRelationshipsComponent {...args} />;
};

GetRemoteDatabaseRelationships.args = {
  database: 'default',
  qualifiedTable: {
    schema: 'public',
    name: 'test',
  },
};

GetRemoteDatabaseRelationships.parameters = {
  // Disable storybook for Remote Relationship stories
  chromatic: { disableSnapshot: true },
};

export default {
  title: 'hooks/Remote Database Relationships/Fetch',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof GetRemoteDatabaseRelationshipsComponent>;
