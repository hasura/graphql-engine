import { useState } from 'react';
import { FaColumns, FaDatabase, FaMagic, FaTable } from 'react-icons/fa';
import Skeleton from 'react-loading-skeleton';
import { capitaliseFirstLetter } from '../../../../components/Common/ConfigureTransformation/utils';
import { Button } from '../../../../new-components/Button';
import { CardedTable } from '../../../../new-components/CardedTable';
import { useSuggestedRelationships } from '../../../Data/TrackResources/TrackRelationships/hooks/useSuggestedRelationships';
import { areTablesEqual } from '../../../hasura-metadata-api';
import { Table } from '../../../hasura-metadata-types';
import { getTableDisplayName } from '../../utils/helpers';
import { RelationshipIcon } from '../RelationshipIcon';
import { SuggestedRelationshipTrackModal } from '../SuggestedRelationshipTrackModal/SuggestedRelationshipTrackModal';
import { SuggestedRelationshipWithName } from './hooks/useSuggestedRelationships';

type SuggestedRelationshipsProps = {
  dataSourceName: string;
  table: Table;
};

export const SuggestedRelationships = ({
  dataSourceName,
  table,
}: SuggestedRelationshipsProps) => {
  const { data: { untracked = [] } = {}, isLoading } =
    useSuggestedRelationships({
      dataSourceName,
      which: 'all',
    });

  const untrackedSuggestedRelationships = untracked.filter(rel =>
    areTablesEqual(rel.from.table, table)
  );

  const [isModalVisible, setModalVisible] = useState(false);
  const [selectedRelationship, setSelectedRelationship] =
    useState<SuggestedRelationshipWithName | null>(null);

  if (isLoading) return <Skeleton count={4} height={30} />;

  return untrackedSuggestedRelationships.length > 0 ? (
    <>
      <CardedTable.Table>
        <CardedTable.Header
          columns={[
            <div>
              <FaMagic className="fill-muted" /> SUGGESTED RELATIONSHIPS
            </div>,
            'SOURCE',
            'TYPE',
            'RELATIONSHIP',
          ]}
        />

        <CardedTable.TableBody>
          {untrackedSuggestedRelationships.map(relationship => (
            <CardedTable.TableBodyRow key={relationship.constraintName}>
              <CardedTable.TableBodyCell>
                <div className="flex flex-row items-center">
                  <Button
                    size="sm"
                    onClick={() => {
                      setSelectedRelationship(relationship);
                      setModalVisible(true);
                    }}
                  >
                    Add
                  </Button>
                  <div className="ml-2">{relationship.constraintName}</div>
                </div>
              </CardedTable.TableBodyCell>

              <CardedTable.TableBodyCell>
                <div className="flex items-center gap-2">
                  <FaDatabase /> <span>{dataSourceName}</span>
                </div>
              </CardedTable.TableBodyCell>

              <CardedTable.TableBodyCell>
                {capitaliseFirstLetter(relationship.type)}
              </CardedTable.TableBodyCell>

              <CardedTable.TableBodyCell>
                <div className="flex flex-row items-center gap-2">
                  <FaTable />
                  <span>{getTableDisplayName(relationship.from.table)}</span>
                  /
                  <FaColumns />
                  <span>{relationship.from.columns.join(' ')}</span>
                  <RelationshipIcon
                    type={
                      relationship.type === 'array'
                        ? 'one-to-many'
                        : 'one-to-one'
                    }
                  />
                  <FaTable />
                  <span>{getTableDisplayName(relationship.to.table)}</span>
                  /
                  <FaColumns />
                  {relationship.to.columns.join(' ')}
                </div>
              </CardedTable.TableBodyCell>
            </CardedTable.TableBodyRow>
          ))}
        </CardedTable.TableBody>
      </CardedTable.Table>
      {isModalVisible && selectedRelationship && (
        <SuggestedRelationshipTrackModal
          relationship={selectedRelationship}
          dataSourceName={dataSourceName}
          onClose={() => setModalVisible(false)}
        />
      )}
    </>
  ) : null;
};
