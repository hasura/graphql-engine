import { useMetadata } from '../../hasura-metadata-api';
import { MetadataSelectors } from '../../hasura-metadata-api';
import { getSupportsForeignKeys } from '../../hasura-metadata-api/utils';
import { Table } from '../../hasura-metadata-types';
import React from 'react';
import {
  TableColumns,
  TableComments,
  TableRootFields,
  ForeignKeys,
} from './components';
import { Section } from './parts';
import { ApolloFederation } from './components/ApolloFederation';
import { useApolloFederationSupportedDrivers } from './hooks/useApolloFederationSupportedDrivers';

export type ModifyTableProps = {
  dataSourceName: string;
  table: Table;
  tableName: string;
};

export const ModifyTable: React.VFC<ModifyTableProps> = props => {
  const { data: source } = useMetadata(
    MetadataSelectors.findSource(props.dataSourceName)
  );

  const supportedDriversForApolloFederation =
    useApolloFederationSupportedDrivers();

  const supportsForeignKeys = getSupportsForeignKeys(source);

  return (
    <div className="w-full bg-white p-4 rounded-sm border my-2">
      <Section headerText="Table Comments">
        <TableComments {...props} />
      </Section>
      <Section headerText="Table Columns">
        <TableColumns {...props} />
      </Section>
      {supportsForeignKeys && (
        <Section
          headerText="Foreign Keys"
          tooltipMessage={`
        Foreign keys are one or more columns that point to another table's primary key. They link both tables.
        `}
        >
          <ForeignKeys {...props} />
        </Section>
      )}
      <Section
        headerText="Custom Field Names"
        tooltipMessage="Customize table and column root names for GraphQL operations."
      >
        <TableRootFields {...props} />
      </Section>

      {supportedDriversForApolloFederation.includes(source?.kind ?? '') && (
        <ApolloFederation
          dataSourceName={props.dataSourceName}
          table={props.table}
        />
      )}
    </div>
  );
};
