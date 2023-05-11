import {
  isSchemaTable,
  isDatasetTable,
  isGDCTable,
} from '../../../DataSource/utils';
import { Table } from '../../../hasura-metadata-types';
import { useCallback } from 'react';
import { useManageLocalRelationship } from '../../hooks/useManageLocalRelationship';
import { useManageRemoteDatabaseRelationship } from '../../hooks/useManageRemoteDatabaseRelationship';
import { useManageRemoteSchemaRelationship } from '../../hooks/useManageRemoteSchemaRelationship';
import {
  LocalRelationship,
  MODE,
  RemoteDatabaseRelationship,
  RemoteSchemaRelationship,
} from '../../types';
import { generateLhsFields } from './parts/MapRemoteSchemaFields/utils';
import { Schema } from './schema';
import { useDriverRelationshipSupport } from '../../../Data/hooks/useDriverRelationshipSupport';
import { hasuraToast } from '../../../../new-components/Toasts';

export const getTableLabel = ({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) => {
  if (isSchemaTable(table)) {
    return `${dataSourceName} / ${table.schema} / ${table.name}`;
  }

  if (isDatasetTable(table)) {
    return `${dataSourceName} / ${table.dataset} / ${table.name}`;
  }

  if (isGDCTable(table)) {
    return `${dataSourceName} / ${table.join(' /')}`;
  }

  return '';
};

export const useHandleSubmit = ({
  dataSourceName,
  table,
  mode,
  onSuccess,
  onError,
}: {
  dataSourceName: string;
  table: Table;
  mode: MODE;
  onSuccess?: () => void;
  onError?: (err: Error) => void;
}) => {
  const { createRelationship: createLocalRelationship, isLoading } =
    useManageLocalRelationship({
      dataSourceName,
      table,
      onSuccess,
      onError,
    });

  const {
    createRelationship: createRemoteDatabaseRelationship,
    editRelationship: editRemoteDatabaseRelationship,
    isLoading: remoteDatabaseRelationshipLoading,
  } = useManageRemoteDatabaseRelationship({
    dataSourceName,
    onSuccess,
    onError,
  });

  const {
    createRelationship: createRemoteSchemaRelationship,
    editRelationship: editRemoteSchemaRelationship,
    isLoading: remoteSchemaRelationshipLoading,
  } = useManageRemoteSchemaRelationship({
    dataSourceName,
    onSuccess,
    onError,
  });

  const { driverSupportsLocalRelationship, driverSupportsRemoteRelationship } =
    useDriverRelationshipSupport({
      dataSourceName,
    });
  const handleSubmit = useCallback(
    (formData: Schema) => {
      const { fromSource, toSource, details } = formData;

      if (
        !driverSupportsLocalRelationship &&
        !driverSupportsRemoteRelationship
      ) {
        hasuraToast({
          type: 'error',
          title: 'Not able to track',
          message: `This datasource does not support tracking of relationships.`,
        });
        return;
      }

      // remote database relationship
      if (
        driverSupportsRemoteRelationship &&
        toSource.type === 'table' &&
        'columnMap' in details
      ) {
        const remoteDatabaseRelationship: RemoteDatabaseRelationship = {
          name: formData.name,
          type: 'remoteDatabaseRelationship',
          fromSource: fromSource.dataSourceName,
          fromTable: fromSource.table,
          relationshipType: details.relationshipType,
          definition: {
            toSource: toSource.dataSourceName,
            toTable: toSource.table,
            mapping: (details.columnMap ?? []).reduce(
              (acc, entry) => ({ ...acc, [entry.from]: entry.to }),
              {}
            ),
          },
        };

        if (mode === MODE.CREATE)
          createRemoteDatabaseRelationship(remoteDatabaseRelationship);
        else editRemoteDatabaseRelationship(remoteDatabaseRelationship);
      } else if (
        toSource.type === 'table' &&
        toSource.dataSourceName === dataSourceName &&
        'columnMap' in details &&
        driverSupportsLocalRelationship
      ) {
        const localRelationship: LocalRelationship = {
          name: formData.name,
          type: 'localRelationship',
          fromSource: fromSource.dataSourceName,
          fromTable: fromSource.table,
          relationshipType: details.relationshipType,
          definition: {
            toTable: toSource.table,
            mapping: (details.columnMap ?? []).reduce(
              (acc, entry) => ({ ...acc, [entry.from]: entry.to }),
              {}
            ),
          },
        };
        createLocalRelationship(localRelationship);
      }

      // remote schema relationship
      if (toSource.type === 'remoteSchema' && 'rsFieldMapping' in details) {
        const remoteSchemaRelationship: RemoteSchemaRelationship = {
          name: formData.name,
          type: 'remoteSchemaRelationship',
          relationshipType: 'Remote',
          fromSource: fromSource.dataSourceName,
          fromTable: fromSource.table,
          definition: {
            toRemoteSchema: toSource.remoteSchema,
            lhs_fields: generateLhsFields(details.rsFieldMapping),
            remote_field: details.rsFieldMapping,
          },
        };
        if (mode === MODE.CREATE)
          createRemoteSchemaRelationship(remoteSchemaRelationship);
        else editRemoteSchemaRelationship(remoteSchemaRelationship);
      }
    },
    [
      createLocalRelationship,
      createRemoteDatabaseRelationship,
      createRemoteSchemaRelationship,
      dataSourceName,
      editRemoteDatabaseRelationship,
      editRemoteSchemaRelationship,
      mode,
      driverSupportsLocalRelationship,
      driverSupportsRemoteRelationship,
    ]
  );

  return {
    handleSubmit,
    isLoading:
      isLoading ||
      remoteDatabaseRelationshipLoading ||
      remoteSchemaRelationshipLoading,
  };
};
