import React from 'react';
import { RightContainer } from '@/components/Common/Layout/RightContainer';
import { Button } from '@/new-components/Button';
import { useFireNotification } from '@/new-components/Notifications';
import { getConfirmation } from '@/components/Common/utils/jsUtils';
import { Driver } from '@/dataSources';
import { FaPlusCircle } from 'react-icons/fa';

import { NormalizedTable } from '@/dataSources/types';
import { DataSourceDriver, getDataSourcePrefix } from '@/metadata/queryUtils';

import TableHeader from '../../components/Services/Data/TableCommon/TableHeader';
import { FeatureFlagFloatingButton } from '../FeatureFlags/components/FeatureFlagFloatingButton';
import {
  DatabaseRelationshipsTable,
  OnClickHandlerArgs,
  RowData,
} from '../RelationshipsTable/DatabaseRelationshipsTable';
import {
  allowedMetadataTypes,
  DbToDbRelationship,
  DbToRemoteSchemaRelationship,
  useMetadataMigration,
} from '../MetadataAPI';
import { DataTarget } from '../Datasources';
import { Form } from './components/Form/Form';

const createTable = (target?: DataTarget) => {
  if (!target) {
    return {};
  }

  if ('schema' in target) {
    return {
      name: target.table,
      schema: target.schema,
    };
  }

  if ('dataset' in target) {
    return {
      name: target.table,
      schema: target.dataset,
    };
  }

  return {
    name: target.table,
  };
};

const useFormState = (currentSource: string) => {
  const [isOpen, setIsOpen] = React.useState(false);
  const [
    existingRelationship,
    setExistingRelationship,
  ] = React.useState<RowData>();

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

  const onEdit = (row?: RowData) => {
    setExistingRelationship(row);
    setIsOpen(true);
  };
  const onCancel = () => {
    setExistingRelationship(undefined);
    setIsOpen(false);
  };

  const onDelete = (row?: RowData) => {
    setExistingRelationship(undefined);
    setIsOpen(false);
    const confirmMessage = `This will permanently delete the ${row?.name} from Hasura`;
    const isOk = getConfirmation(confirmMessage, true, row?.name);
    if (!isOk) {
      return;
    }

    const sourcePrefix = getDataSourcePrefix(currentSource as DataSourceDriver);

    if (row?.toType === 'remote_schema') {
      return mutation.mutate({
        query: {
          type: `${sourcePrefix}delete_remote_relationship` as allowedMetadataTypes,
          args: {
            name: row?.name,
            source: currentSource,
            table: createTable(
              (row?.relationship as DbToRemoteSchemaRelationship)?.target
            ),
          },
        },
      });
    } else if (row?.toType === 'database') {
      return mutation.mutate({
        query: {
          type: `${sourcePrefix}delete_remote_relationship` as allowedMetadataTypes,
          args: {
            name: row?.name,
            source: currentSource,
            table: createTable(
              (row?.relationship as DbToDbRelationship)?.target
            ),
          },
        },
      });
    } else if (row?.toType === 'table') {
      return mutation.mutate({
        query: {
          type: `${sourcePrefix}drop_relationship` as allowedMetadataTypes,
          args: {
            relationship: row?.name,
            table: row.referenceTable,
            source: row.reference,
          },
        },
      });
    }
  };

  const onClick = ({ type, row }: OnClickHandlerArgs) => {
    switch (type) {
      case 'add':
        onOpen();
        break;
      case 'close':
        onCancel();
        break;
      case 'edit':
        onEdit(row);
        break;
      case 'delete':
        onDelete(row);
        break;
      default:
        setIsOpen(false);
    }
  };

  return { isOpen, onClick, existingRelationship };
};

export const DatabaseRelationshipsTab = ({
  table,
  currentSource,
  migrationMode,
  driver,
}: {
  table: NormalizedTable;
  currentSource: string;
  migrationMode: boolean;
  driver: Driver;
}) => {
  const { isOpen, existingRelationship, onClick } = useFormState(currentSource);
  const onComplete = ({
    type,
  }: {
    title?: string;
    message?: string;
    type: 'success' | 'error' | 'cancel';
  }) => {
    if (type === 'success' || type === 'cancel') {
      onClick({ type: 'close' });
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
        target={
          {
            database: currentSource,
            table: table.table_name,
            [driver === 'bigquery' ? 'dataset' : 'schema']: table.table_schema, // TODO find a better way to handle this so that GDC can work
            kind: driver,
          } as DataTarget
        }
        onClick={onClick}
      />

      {isOpen ? null : (
        <Button
          onClick={() => onClick({ type: 'add' })}
          icon={<FaPlusCircle />}
        >
          Add New Relationship
        </Button>
      )}

      {isOpen && (
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
      )}
      <FeatureFlagFloatingButton />
    </RightContainer>
  );
};
