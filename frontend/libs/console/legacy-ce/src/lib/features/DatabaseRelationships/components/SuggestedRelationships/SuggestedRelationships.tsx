import { useState } from 'react';
import { FaDatabase, FaMagic } from 'react-icons/fa';
import { Button, CardedTable, Text } from '@hasura/shared/ui';
import { useSuggestedRelationships } from '@hasura/metadata/api';
import { Table } from '@hasura/shared/types';
import { SuggestedRelationshipTrackModal } from '../SuggestedRelationshipTrackModal/SuggestedRelationshipTrackModal';
import { SuggestedRelationshipWithName } from './hooks/useSuggestedRelationships';
import { areTablesEqual } from '@hasura/metadata/helpers';
import { Flex } from '@radix-ui/themes';
import { capitalizeFirstLetter } from '@hasura/shared/utils';
import { DisplaySuggestedRelationship } from '../common/mapping/DisplaySuggestedRelationship';

const COLUMNS = [
  <Flex align="center" gap="2" key="suggested-header">
    <FaMagic /> SUGGESTED RELATIONSHIPS
  </Flex>,
  'SOURCE',
  'TYPE',
  'RELATIONSHIP',
];

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

  const untrackedSuggestedRelationships = untracked.filter((rel) =>
    areTablesEqual(rel.from.table, table),
  );

  const [isModalVisible, setModalVisible] = useState(false);
  const [selectedRelationship, setSelectedRelationship] =
    useState<SuggestedRelationshipWithName | null>(null);

  if (isLoading)
    return (
      <CardedTable columns={COLUMNS} data={[]} loading loadingRowCount={4} />
    );

  return untrackedSuggestedRelationships.length > 0 ? (
    <>
      <CardedTable
        columns={COLUMNS}
        data={untrackedSuggestedRelationships.map((relationship) => [
          <Flex
            direction="row"
            align="center"
            gap="2"
            key={`${relationship.constraintName}-add`}
          >
            <Button
              mode="default"
              size="sm"
              onClick={() => {
                setSelectedRelationship(relationship);
                setModalVisible(true);
              }}
            >
              Add
            </Button>
            <Text>{relationship.constraintName}</Text>
          </Flex>,
          <Flex
            align="center"
            gap="2"
            key={`${relationship.constraintName}-source`}
          >
            <FaDatabase /> <span>{dataSourceName}</span>
          </Flex>,
          capitalizeFirstLetter(relationship.type),
          <DisplaySuggestedRelationship
            key={`${relationship.constraintName}-mapping`}
            relationship={relationship}
          />,
        ])}
      />
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
