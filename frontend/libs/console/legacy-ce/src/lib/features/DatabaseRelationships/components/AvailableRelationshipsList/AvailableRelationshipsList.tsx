import { Flex } from '@radix-ui/themes';
import { CardedTable, IndicatorCard, SkeletonList } from '@hasura/shared/ui';
import { Table } from '@hasura/shared/types';
import { useListAllDatabaseRelationships } from '../../hooks/useListAllDatabaseRelationships';
import { MODE, Relationship } from '../../types';
import { RelationshipMapping } from './parts/RelationshipMapping';
import { RowActions } from './parts/RowActions';
import { TargetName } from './parts/TargetName';

export interface AvailableRelationshipsListProps {
  dataSourceName: string;
  onAction: (relationship: Relationship, mode: MODE) => void;
  table: Table;
}

export const AvailableRelationshipsList = ({
  dataSourceName,
  onAction,
  table,
}: AvailableRelationshipsListProps) => {
  const { data: relationships } = useListAllDatabaseRelationships({
    dataSourceName,
    table,
  });

  if (!relationships) return <SkeletonList count={7} />;

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
        columns={['NAME', 'SOURCE', 'TYPE', 'RELATIONSHIP', <></>]}
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
