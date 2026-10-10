import { NativeDriver } from '@hasura/shared/types';
import React from 'react';
import {
  TableColumns,
  TableComments,
  TableRootFields,
  ForeignKeys,
  ComputedFields,
  SetAsEnum,
  CheckConstraints,
  PrimaryKey,
  UniqueKeys,
  Indexes,
  Triggers,
} from './components';
import { Section } from './parts';
import { ApolloFederation } from './components/ApolloFederation';
import { useApolloFederationSupportedDrivers } from './hooks/useApolloFederationSupportedDrivers';
import { isPostgresFlavour, MetadataSelectors } from '@hasura/metadata/helpers';
import { getDatabaseMethods } from '@hasura/metadata/data-source';
import { ModifyTableProps } from './types';
import ViewDefinition from './components/ViewDefinition';
import { useAppContext } from '@hasura/shared/context';

export const ModifyTable: React.FC<ModifyTableProps> = (props) => {
  const { readOnlyMode } = useAppContext();
  const supportedDriversForApolloFederation =
    useApolloFederationSupportedDrivers();

  const databaseMethods = getDatabaseMethods(props.source.kind);
  const supportsForeignKeys = MetadataSelectors.getSupportsForeignKeys(
    props.source,
  );
  const supportsComputedFields = isPostgresFlavour(props.source.kind);
  const supportsSetAsEnum =
    !props.isView &&
    databaseMethods.check.isFeatureSupported('tables.modify.setAsEnum');
  const supportsCheckConstraints =
    !props.isView && Boolean(databaseMethods.modify?.createCheckConstraint);
  const supportsPrimaryKey =
    !props.isView &&
    Boolean(
      databaseMethods.modify?.createPrimaryKey ||
      databaseMethods.modify?.alterPrimaryKey,
    );
  const supportsUniqueKeys =
    !props.isView && Boolean(databaseMethods.modify?.createUniqueKey);
  const supportsIndexes =
    !props.isView && Boolean(databaseMethods.introspection.getTableIndexes);
  const supportsTriggers =
    !props.isView && Boolean(databaseMethods.introspection.getTableTriggers);

  return (
    <div className="w-full p-4 md:w-8/12">
      {props.isView ? (
        <ViewDefinition
          source={props.source}
          table={props.table.table}
          readOnly={readOnlyMode}
        />
      ) : null}
      <Section headerText="Table Columns">
        <TableColumns {...props} />
      </Section>
      {supportsForeignKeys && !props.isView && (
        <Section
          headerText="Foreign Keys"
          tooltipMessage={`
        Foreign keys are one or more columns that point to another table's primary key. They link both tables.
        `}
        >
          <ForeignKeys {...props} />
        </Section>
      )}
      {supportsPrimaryKey && (
        <Section
          headerText="Primary Key"
          tooltipMessage="Set or replace the table's primary key."
        >
          <PrimaryKey {...props} />
        </Section>
      )}
      {supportsUniqueKeys && (
        <Section
          headerText="Unique Keys"
          tooltipMessage="Add or remove unique constraints on this table."
        >
          <UniqueKeys {...props} />
        </Section>
      )}
      {supportsIndexes && (
        <Section
          headerText="Indexes"
          tooltipMessage="List, add, or remove indexes on this table."
        >
          <Indexes {...props} />
        </Section>
      )}
      {supportsTriggers && (
        <Section
          headerText="Triggers"
          tooltipMessage="List and remove database triggers on this table."
        >
          <Triggers {...props} />
        </Section>
      )}
      <Section
        headerText="Custom Field Names"
        tooltipMessage="Customize table and column root names for GraphQL operations."
      >
        <TableRootFields {...props} />
      </Section>

      {supportsComputedFields && (
        <Section
          headerText="Computed Fields"
          tooltipMessage="Add a function as a virtual field in the GraphQL API"
        >
          <ComputedFields {...props} />
        </Section>
      )}

      {supportsCheckConstraints && (
        <Section
          headerText="Check Constraints"
          tooltipMessage="A check constraint allows you to specify if the value in a certain column must satisfy a specific condition."
        >
          <CheckConstraints {...props} />
        </Section>
      )}

      {supportsSetAsEnum && <SetAsEnum {...props} />}
      <TableComments {...props} />

      {supportedDriversForApolloFederation.includes(
        props.source.kind as NativeDriver,
      ) && <ApolloFederation {...props} />}
    </div>
  );
};
