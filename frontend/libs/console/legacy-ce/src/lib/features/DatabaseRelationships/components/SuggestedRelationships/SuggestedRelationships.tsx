import { useState } from 'react';
import { FaDatabase, FaMagic } from 'react-icons/fa';
import { Button, CardedTable, SkeletonList, Text } from '@hasura/shared/ui';
import { useSuggestedRelationships } from '@hasura/metadata/api';
import { Table } from '@hasura/shared/types';
import { SuggestedRelationshipTrackModal } from '../SuggestedRelationshipTrackModal/SuggestedRelationshipTrackModal';
import { SuggestedRelationshipWithName } from './hooks/useSuggestedRelationships';
import { areTablesEqual } from '@hasura/metadata/helpers';
import { Flex } from '@radix-ui/themes';
import { capitalizeFirstLetter } from '@hasura/shared/utils';
import { DisplaySuggestedRelationship } from '../common/mapping/DisplaySuggestedRelationship';

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

  if (isLoading) return <SkeletonList count={4} />;

  return untrackedSuggestedRelationships.length > 0 ? (
    <>
      <CardedTable
        columns={[
          <Flex align="center" gap="2" key="suggested-header">
            <FaMagic /> SUGGESTED RELATIONSHIPS
          </Flex>,
          'SOURCE',
          'TYPE',
          'RELATIONSHIP',
        ]}
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
