import { useState } from 'react';
import { capitaliseFirstLetter } from '@/components/Common/ConfigureTransformation/utils';
import { Table } from '@/features/hasura-metadata-types';
import { Button } from '@/new-components/Button';
import { CardedTable } from '@/new-components/CardedTable';
import { useFireNotification } from '@/new-components/Notifications';
import {
  FaArrowRight,
  FaColumns,
  FaDatabase,
  FaMagic,
  FaTable,
} from 'react-icons/fa';
import { MetadataSelectors, useMetadata } from '@/features/hasura-metadata-api';
import { getSupportsForeignKeys } from '@/features/hasura-metadata-api/utils';
import { useListAllDatabaseRelationships } from '../../hooks/useListAllDatabaseRelationships';
import { useManageLocalRelationship } from '../../hooks/useManageLocalRelationship';
import { getTableDisplayName } from '../../utils/helpers';
import {
  SuggestedRelationshipWithName,
  useSuggestedRelationships,
} from './hooks/useSuggestedRelationships';
import { convertSuggestedRelationShipToLocalRelationship } from './SuggestedRelationships.utils';
import type { LocalRelationship } from '../../types';
import Skeleton from 'react-loading-skeleton';

type SuggestedRelationshipsProps = {
  dataSourceName: string;
  table: Table;
};

export const SuggestedRelationships = ({
  dataSourceName,
  table,
}: SuggestedRelationshipsProps) => {
  const { data: existingRelationships } = useListAllDatabaseRelationships({
    dataSourceName,
    table,
  });

  const localRelationships = existingRelationships.filter(rel => {
    if (rel.type === 'localRelationship') {
      return true;
    }
    return false;
  }) as LocalRelationship[];

  const { data: source } = useMetadata(
    MetadataSelectors.findSource(dataSourceName)
  );

  const supportsForeignKeys = getSupportsForeignKeys(source);

  const {
    suggestedRelationships,
    isLoadingSuggestedRelationships,
    refetchSuggestedRelationships,
  } = useSuggestedRelationships({
    dataSourceName,
    table,
    existingRelationships: localRelationships,
    isEnabled: supportsForeignKeys,
  });

  const { fireNotification } = useFireNotification();

  const { createRelationship, isLoading: isCreatingRelationship } =
    useManageLocalRelationship({
      dataSourceName,
      table,
      onSuccess: () => {
        fireNotification({
          title: 'Success',
          message: 'Relationship tracked',
          type: 'success',
        });
        refetchSuggestedRelationships();
      },
      onError: () => {
        fireNotification({
          title: 'Error',
          message: 'An error occurred',
          type: 'error',
        });
      },
    });

  const [updatedRelationship, setUpdatedRelationship] = useState<string | null>(
    null
  );
  const onCreate = (relationship: SuggestedRelationshipWithName) => {
    setUpdatedRelationship(relationship.constraintName);
    createRelationship(
      convertSuggestedRelationShipToLocalRelationship(
        dataSourceName,
        relationship
      )
    );
  };

  if (isLoadingSuggestedRelationships)
    return <Skeleton count={4} height={30} />;

  return suggestedRelationships.length > 0 ? (
    <CardedTable.Table>
      <CardedTable.Header
        columns={[
          <div>
            <FaMagic className="fill-muted" /> FOREIGN KEY RELATIONSHIPS
          </div>,
          'SOURCE',
          'TYPE',
          'RELATIONSHIP',
        ]}
      />

      <CardedTable.TableBody>
        {suggestedRelationships.map(relationship => {
          const relationshipName = relationship.constraintName;
          return (
            <CardedTable.TableBodyRow key={relationshipName}>
              <CardedTable.TableBodyCell>
                <div className="flex flex-row items-center">
                  <Button
                    size="sm"
                    onClick={() => onCreate(relationship)}
                    isLoading={
                      isCreatingRelationship &&
                      updatedRelationship === relationship.constraintName
                    }
                  >
                    Add
                  </Button>
                  <div className="ml-2">{relationshipName}</div>
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
          );
        })}
      </CardedTable.TableBody>
    </CardedTable.Table>
  ) : null;
};
