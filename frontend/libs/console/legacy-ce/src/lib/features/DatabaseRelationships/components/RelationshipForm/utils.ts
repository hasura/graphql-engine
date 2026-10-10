import {
  BulkAtomicResponse,
  BulkKeepGoingResponse,
  Table,
} from '@hasura/shared/types';
import { useCallback } from 'react';
import { MODE } from '../../types';
import { Schema } from './schema';
import { generateLhsFields } from '../../../RemoteRelationships/utils';
import { useCreateTableRelationships } from '@hasura/metadata/data-source';

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
  onSuccess?: (data: BulkAtomicResponse | BulkKeepGoingResponse) => void;
  onError?: (err: Error) => void;
}) => {
  const { createTableRelationships, ...rest } = useCreateTableRelationships(
    dataSourceName,
    {
      onSuccess,
      onError,
    },
  );

  const handleSubmit = useCallback(
    (formData: Schema) => {
      const { fromSource, toSource, details } = formData;

      if (toSource.type === 'remoteSchema' && 'rsFieldMapping' in details) {
        createTableRelationships([
          {
            name: formData.name,
            source: {
              fromSource: fromSource.dataSourceName,
              fromTable: fromSource.table,
            },
            isEditMode: mode === MODE.EDIT,
            definition: {
              target: {
                toRemoteSchema: toSource.remoteSchema,
              },
              detail: {
                lhs_fields: generateLhsFields(details.rsFieldMapping),
                remote_field: details.rsFieldMapping,
              },
            },
          },
        ]);
        return;
      }

      /**
       * Same database relationship
       */
      if (
        toSource.type === 'table' &&
        fromSource.dataSourceName === toSource.dataSourceName &&
        'columnMap' in details
      ) {
        createTableRelationships([
          {
            name: formData.name,
            source: {
              fromSource: fromSource.dataSourceName,
              fromTable: fromSource.table,
            },
            isEditMode: mode === MODE.EDIT,
            definition: {
              target: {
                toSource: toSource.dataSourceName,
                toTable: toSource.table,
              },
              type: details.relationshipType === 'Object' ? 'object' : 'array',
              detail: {
                columnMapping: (details.columnMap ?? []).reduce(
                  (acc, entry) => ({ ...acc, [entry.from]: entry.to }),
                  {},
                ),
              },
            },
          },
        ]);
        return;
      }

      /**
       * remote database relationship
       */
      if (
        toSource.type === 'table' &&
        fromSource.dataSourceName !== toSource.dataSourceName &&
        'columnMap' in details
      ) {
        createTableRelationships([
          {
            name: formData.name,
            source: {
              fromSource: fromSource.dataSourceName,
              fromTable: fromSource.table,
            },
            isEditMode: mode === MODE.EDIT,
            definition: {
              target: {
                toRemoteSource: toSource.dataSourceName,
                toRemoteTable: toSource.table,
              },
              type: details.relationshipType === 'Object' ? 'object' : 'array',
              detail: {
                columnMapping: (details.columnMap ?? []).reduce(
                  (acc, entry) => ({ ...acc, [entry.from]: entry.to }),
                  {},
                ),
              },
            },
          },
        ]);
        return;
      }
    },
    [createTableRelationships, mode],
  );

  return {
    handleSubmit,
    ...rest,
  };
};
