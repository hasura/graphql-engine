import React from 'react';
import { RightContainer } from '@/components/Common/Layout/RightContainer';
import { Button } from '@/new-components/Button';
import { useFireNotification } from '@/new-components/Notifications';
import { useQueryClient } from 'react-query';
import { getConfirmation } from '@/components/Common/utils/jsUtils';
import { Driver } from '@/dataSources';
import { FaPlusCircle } from 'react-icons/fa';
import { NormalizedTable } from '@/dataSources/types';
import { DataSourceDriver, getDataSourcePrefix } from '@/metadata/queryUtils';
import TableHeader from '../../components/Services/Data/TableCommon/TableHeader';
import { FeatureFlagFloatingButton } from '../FeatureFlags/components/FeatureFlagFloatingButton';
import { DatabaseRelationshipsTable } from '../RelationshipsTable/DatabaseRelationshipsTable';
import { allowedMetadataTypes, useMetadataMigration } from '../MetadataAPI';
import { Form } from './components/Form/Form';
import { Relationship } from '../RelationshipsTable/DatabaseRelationshipsTable/types';
import { Table } from '../DataSource';

const useFormState = (currentSource: string) => {
  const [isOpen, setIsOpen] = React.useState(false);
  const [existingRelationship, setExistingRelationship] =
    React.useState<Relationship>();

  const { fireNotification } = useFireNotification();
  const mutation = useMetadataMigration({
    onSuccess: () => {
      fireNotification({
        title: 'Success!',
        message: 'Relationship deleted successfully',
        type: 'success',
      });
    },
    onError: () => {
      fireNotification({
        title: 'Error',
        message: 'Error while deleting the relationship',
        type: 'error',
      });
    },
  });

  const onOpen = () => {
    setExistingRelationship(undefined);
    setIsOpen(true);
  };

  const editRelationship = (relationship: Relationship) => {
    setExistingRelationship(relationship);
    setIsOpen(true);
  };

  const closeForm = () => {
    setExistingRelationship(undefined);
    setIsOpen(false);
  };

  const deleteRelationship = (row: Relationship) => {
    setExistingRelationship(undefined);
    setIsOpen(false);
    const confirmMessage = `This will permanently delete the ${row?.name} from Hasura`;
    const isOk = getConfirmation(confirmMessage, true, row?.name);
    if (!isOk) {
      return;
    }

    const sourcePrefix = getDataSourcePrefix(currentSource as DataSourceDriver);

    if (row.type === 'toRemoteSchema') {
      mutation.mutate({
        query: {
          type: `${sourcePrefix}delete_remote_relationship` as allowedMetadataTypes,
          args: {
            name: row.name,
            source: currentSource,
            table: row.mapping.from.table,
          },
        },
      });
      return;
    }

    if (row.type === 'toSource') {
      mutation.mutate({
        query: {
          type: `${sourcePrefix}delete_remote_relationship` as allowedMetadataTypes,
          args: {
            name: row.name,
            source: currentSource,
            table: row.mapping.from.table,
          },
        },
      });
      return;
    }

    /**
     * It must be a local/self table relationship
     */
    mutation.mutate({
      query: {
        type: `${sourcePrefix}drop_relationship` as allowedMetadataTypes,
        args: {
          relationship: row.name,
          table: row.toLocalTable,
          source: currentSource,
        },
      },
    });
  };

  const openForm = () => onOpen();

  const onClick = ({ type, row }: { type: string; row: Relationship }) => {
    switch (type) {
      case 'delete':
        deleteRelationship(row);
        break;
      default:
        setIsOpen(false);
    }
  };

  return {
    isOpen,
    onClick,
    existingRelationship,
    openForm,
    closeForm,
    deleteRelationship,
    editRelationship,
  };
};

export const DatabaseRelationshipsTab = ({
  table,
  currentSource,
  migrationMode,
  driver,
  metadataTable,
}: {
  table: NormalizedTable;
  currentSource: string;
  migrationMode: boolean;
  driver: Driver;
  metadataTable: Table;
}) => {
  const {
    isOpen,
    existingRelationship,
    editRelationship,
    openForm,
    closeForm,
    deleteRelationship,
  } = useFormState(currentSource);

  const queryClient = useQueryClient();

  const onComplete = ({
    type,
  }: {
    title?: string;
    message?: string;
    type: 'success' | 'error' | 'cancel';
  }) => {
    if (type === 'success' || type === 'cancel') {
      closeForm();
      queryClient.refetchQueries([currentSource, 'list_all_relationships'], {
        exact: true,
      });
    }
  };

  return (
    <RightContainer>
      <TableHeader
        dispatch={() => {}}
        table={table}
        source={currentSource}
        tabName="relationships"
        migrationMode={migrationMode}
        readOnlyMode={false}
        count={null}
        isCountEstimated
      />

      <div className="py-4">
        <h2 className="text-md font-semibold">Data Relationships</h2>
      </div>

      <DatabaseRelationshipsTable
        dataSourceName={currentSource}
        table={metadataTable}
        onEditRow={({ relationship }) => {
          editRelationship(relationship);
        }}
        onDeleteRow={({ relationship }) => {
          deleteRelationship(relationship);
        }}
      />

      {isOpen ? null : (
        <Button onClick={() => openForm()} icon={<FaPlusCircle />}>
          Add New Relationship
        </Button>
      )}

      {isOpen ? (
        <Form
          existingRelationship={existingRelationship}
          sourceTableInfo={{
            database: currentSource,
            [driver === 'bigquery' ? 'dataset' : 'schema']: table.table_schema, // TODO find a better way to handle this so that GDC can work
            table: table.table_name,
          }}
          onComplete={onComplete}
          driver={driver}
        />
      ) : null}
      <FeatureFlagFloatingButton />
    </RightContainer>
  );
};
