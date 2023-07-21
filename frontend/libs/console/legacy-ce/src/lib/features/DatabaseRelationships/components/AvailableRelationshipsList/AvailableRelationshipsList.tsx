import { FiRefreshCcw } from 'react-icons/fi';
import Skeleton from 'react-loading-skeleton';
import { CardedTable } from '../../../../new-components/CardedTable';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { useInvalidateMetadata } from '../../../hasura-metadata-api';
import { Table } from '../../../hasura-metadata-types';
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

  const invalidateMetadata = useInvalidateMetadata();

  const refreshMetadata = () => {
    invalidateMetadata({
      componentName: 'AvailableRelationshipList',
      reasons: ['User clicked button to refresh metadata.'],
    });
  };

  if (!relationships) return <Skeleton count={7} height={30} />;

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
      <CardedTable.Table>
        <CardedTable.Header
          columns={[
            'NAME',
            'SOURCE',
            'TYPE',
            'RELATIONSHIP',
            <div className="flex justify-end hidden">
              <FiRefreshCcw onClick={() => refreshMetadata()} />
            </div>,
          ]}
        />

        <CardedTable.TableBody>
          {relationships.map(relationship => (
            <CardedTable.TableBodyRow key={relationship.name}>
              <CardedTable.TableBodyCell>
                {relationship.name}
              </CardedTable.TableBodyCell>

              <CardedTable.TableBodyCell>
                <div className="flex items-center gap-2">
                  <TargetName relationship={relationship} />
                </div>
              </CardedTable.TableBodyCell>

              <CardedTable.TableBodyCell>
                {relationship.relationshipType}
              </CardedTable.TableBodyCell>

              <CardedTable.TableBodyCell>
                <RelationshipMapping relationship={relationship} />
              </CardedTable.TableBodyCell>

              <CardedTable.TableBodyActionCell>
                <RowActions
                  relationship={relationship}
                  onActionClick={onAction}
                />
              </CardedTable.TableBodyActionCell>
            </CardedTable.TableBodyRow>
          ))}
        </CardedTable.TableBody>
      </CardedTable.Table>
    </div>
  );
};
