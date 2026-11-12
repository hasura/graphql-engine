import { Flex } from '@radix-ui/themes';
import { CardedTable, IndicatorCard } from '@hasura/shared/ui';
import { Table } from '@hasura/shared/types';
import { useListAllDatabaseRelationships } from '../../hooks/useListAllDatabaseRelationships';
import { MODE, Relationship } from '../../types';
import { RelationshipMapping } from './parts/RelationshipMapping';
import { RowActions } from './parts/RowActions';
import { TargetName } from './parts/TargetName';

interface AvailableRelationshipsListProps {
  dataSourceName: string;
  onAction: (relationship: Relationship, mode: MODE) => void;
  table: Table;
}

const COLUMNS = ['NAME', 'SOURCE', 'TYPE', 'RELATIONSHIP', ''];

export const AvailableRelationshipsList = ({
  dataSourceName,
  onAction,
  table,
}: AvailableRelationshipsListProps) => {
  const {
    data: relationships,
    isPending,
    error,
  } = useListAllDatabaseRelationships({
    dataSourceName,
    table,
  });

  const fetchError = error.find(Boolean);

  if (fetchError)
    return (
      <IndicatorCard
        status="negative"
        headline="Failed to load the relationships."
        showIcon
      >
        {fetchError.message}
      </IndicatorCard>
    );

  if (isPending) return <CardedTable columns={COLUMNS} data={[]} loading />;

  if (!relationships.length)
    return (
      <IndicatorCard status="info" headline="No Relationships found.">
        No Relationships have been tracked for this table. Refer the{' '}
        <a href="https://hasura.io/docs/latest/index/">docs</a> on how to create
        and use relationships in your GraphQL schema.
      </IndicatorCard>
    );

  return (
    <div>
      <CardedTable
        columns={COLUMNS}
        data={relationships.map((relationship) => [
          relationship.name,
          <Flex align="center" gap="2" key={`${relationship.name}-target`}>
            <TargetName relationship={relationship} />
          </Flex>,
          relationship.relationshipType,
          <RelationshipMapping
            key={`${relationship.name}-mapping`}
            relationship={relationship}
          />,
          <RowActions
            key={`${relationship.name}-actions`}
            relationship={relationship}
            onActionClick={onAction}
          />,
        ])}
      />
    </div>
  );
};
