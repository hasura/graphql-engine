import { useState } from 'react';
import { capitaliseFirstLetter } from '../../../../components/Common/ConfigureTransformation/utils';
import { Table } from '../../../hasura-metadata-types';
import { Button } from '../../../../new-components/Button';
import { CardedTable } from '../../../../new-components/CardedTable';
import {
  FaArrowRight,
  FaColumns,
  FaDatabase,
  FaMagic,
  FaTable,
} from 'react-icons/fa';
import { MetadataSelectors, useMetadata } from '../../../hasura-metadata-api';
import { getSupportsForeignKeys } from '../../../hasura-metadata-api/utils';
import { getTableDisplayName } from '../../utils/helpers';
import {
  SuggestedRelationshipWithName,
  useSuggestedRelationships,
} from './hooks/useSuggestedRelationships';
import Skeleton from 'react-loading-skeleton';
import { SuggestedRelationshipTrackModal } from '../SuggestedRelationshipTrackModal/SuggestedRelationshipTrackModal';

type SuggestedRelationshipsProps = {
  dataSourceName: string;
  table: Table;
};

export const SuggestedRelationships = ({
  dataSourceName,
  table,
}: SuggestedRelationshipsProps) => {
  const { data: source } = useMetadata(
    MetadataSelectors.findSource(dataSourceName)
  );

  const supportsForeignKeys = getSupportsForeignKeys(source);

  const { suggestedRelationships, isLoadingSuggestedRelationships } =
    useSuggestedRelationships({
      dataSourceName,
      table,
      isEnabled: supportsForeignKeys,
    });

  const [isModalVisible, setModalVisible] = useState(false);
  const [selectedRelationship, setSelectedRelationship] =
    useState<SuggestedRelationshipWithName | null>(null);

  if (isLoadingSuggestedRelationships)
    return <Skeleton count={4} height={30} />;

  return suggestedRelationships.length > 0 ? (
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
          {suggestedRelationships.map(relationship => (
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
                  {relationship.from.columns.join(' ')}
                  <FaArrowRight />
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
