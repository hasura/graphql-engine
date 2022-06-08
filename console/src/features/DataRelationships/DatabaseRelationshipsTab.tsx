import React from 'react';
import { RightContainer } from '@/components/Common/Layout/RightContainer';
import { Button } from '@/new-components/Button';
import { fireNotification } from '@/new-components/Notifications';
import { getConfirmation } from '@/components/Common/utils/jsUtils';

import { NormalizedTable } from '@/dataSources/types';

import TableHeader from '../../components/Services/Data/TableCommon/TableHeader';
import { FeatureFlagFloatingButton } from '../FeatureFlags/components/FeatureFlagFloatingButton';
import {
  DatabaseRelationshipsTable,
  OnClickHandlerArgs,
  RowData,
} from '../RelationshipsTable/DatabaseRelationshipsTable';
import { allowedMetadataTypes, useMetadataMigration } from '../MetadataAPI';
import { DataTarget } from '../Datasources';

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

const useFormState = () => {
  const [isOpen, setIsOpen] = React.useState(false);
  const [
    existingRelationship,
    setExistingRelationship,
  ] = React.useState<RowData>();

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
    console.log(row);
    setExistingRelationship(row);
    setIsOpen(true);
  };

  const onDelete = (row?: RowData) => {
    setExistingRelationship(undefined);
    setIsOpen(false);
    const confirmMessage = `This will permanently delete the ${row?.name} from Hasura`;
    const isOk = getConfirmation(confirmMessage, true, row?.name);
    if (!isOk) {
      return;
    }

    if (row && 'relationship' in row && 'target' in row.relationship)
      mutation.mutate({
        query: {
          type: 'delete_remote_relationship' as allowedMetadataTypes,
          args: {
            table: createTable(row?.relationship?.target),
          },
        },
      });
  };

  const onClick = ({ type, row }: OnClickHandlerArgs) => {
    switch (type) {
      case 'add':
        onOpen();
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
  tableSchema,
  currentSource,
}: {
  tableSchema: NormalizedTable;
  currentSource: string;
}) => {
  const { isOpen, existingRelationship, onClick } = useFormState();

  return (
    <RightContainer>
      <TableHeader
        dispatch={() => {}}
        table={tableSchema}
        source={currentSource}
        tabName="relationships"
        migrationMode={false}
        readOnlyMode={false}
        count={null}
        isCountEstimated
      />
      <div className="py-4">
        <h2 className="text-md font-semibold">Data Relationships</h2>
      </div>
      <DatabaseRelationshipsTable
        target={{
          database: currentSource,
          table: tableSchema.table_name,
          schema: tableSchema.table_schema,
        }}
        onClick={onClick}
      />
      <Button mode="primary" onClick={() => onClick({ type: 'add' })}>
        Add Relationship
      </Button>

      {/* to be replaced with actual form */}
      {isOpen && (
        <div className="mt-6">
          Create relationship form here{' '}
          {existingRelationship &&
            `with existing relationship ${existingRelationship.name}`}
        </div>
      )}
      <FeatureFlagFloatingButton />
    </RightContainer>
  );
};
