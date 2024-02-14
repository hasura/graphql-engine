import React from 'react';
import { useGetRemoteSchemaRelationship } from '../../../../../MetadataAPI';
import { rsToDbRelDef } from '../../../../../../metadata/types';

import { Schema } from '../schema';
import {
  useInconsistentMetadata,
  useMetadata,
} from '../../../../../hasura-metadata-api';
import { SourceOption } from '../../RemoteDatabaseWidget/SourceSelect';
import { getTableLabel } from '../../../../../DatabaseRelationships/components/RelationshipForm/utils';
import { useAllDriverCapabilities } from '../../../../../Data/hooks/useAllDriverCapabilities';
import { Feature } from '../../../../../DataSource';
import { isObject } from '../../../../../../components/Common/utils/jsUtils';

interface UseDefaultValuesProps {
  sourceRemoteSchema: string;
  remoteRelationshipName?: string;
  typeName?: string;
}

export const useDefaultValues = ({
  sourceRemoteSchema,
  typeName = '',
  remoteRelationshipName = '',
}: UseDefaultValuesProps) => {
  const { data, isLoading, isError } =
    useGetRemoteSchemaRelationship(sourceRemoteSchema);

  const rsToDbRelationship = data?.remote_relationships?.find(
    ({ type_name: existingTypeName }) => typeName === existingTypeName
  ) as rsToDbRelDef;

  const relationship = rsToDbRelationship?.relationships?.find(
    ({ name }) => name === remoteRelationshipName
  );

  const relationshipInfo = relationship?.definition?.to_source;

  const defaultValues: Schema = React.useMemo(
    () => ({
      relationshipName: relationship?.name || '',
      target: {
        dataSourceName: relationshipInfo?.source || '',
        table: relationshipInfo?.table || {},
        type: 'table',
      },
      mapping: Object.entries(relationshipInfo?.field_mapping ?? {}).map(
        ([field, column]) => ({ field, column: column as string })
      ),
      typeName: rsToDbRelationship?.type_name || '',
      sourceRemoteSchema: sourceRemoteSchema || '',
      relationshipType: relationshipInfo?.relationship_type || 'array',
      driver: '',
    }),
    [relationship, relationshipInfo, rsToDbRelationship, sourceRemoteSchema]
  );

  return { data: defaultValues, isLoading, isError };
};

export const useSourceOptions = () => {
  const { data: inconsistentSources = [], isFetching } =
    useInconsistentMetadata(m => {
      return m.inconsistent_objects
        .filter(item => item.type === 'source')
        .map(source => source.definition);
    });

  const { data: driverCapabilties = [] } = useAllDriverCapabilities({
    select: data => {
      const result = data.map(item => {
        if (item.capabilities === Feature.NotImplemented)
          return {
            driver: item.driver,
            capabilities: {
              isRemoteSchemaRelationshipSupported: false,
            },
          };
        return {
          driver: item.driver,
          capabilities: {
            isRemoteSchemaRelationshipSupported: isObject(
              item.capabilities.queries?.foreach
            ),
          },
        };
      });

      return result;
    },
  });

  return useMetadata(
    m => {
      const tables: SourceOption[] = m.metadata.sources
        .filter(source => !inconsistentSources.includes(source.name))
        .filter(
          source =>
            driverCapabilties?.find(c => c.driver.kind === source?.kind)
              ?.capabilities.isRemoteSchemaRelationshipSupported
        )
        .map(source => {
          return source.tables.map<SourceOption>(t => ({
            value: {
              type: 'table',
              dataSourceName: source.name,
              table: t.table,
            },
            label: getTableLabel({
              dataSourceName: source.name,
              table: t.table,
            }),
          }));
        })
        .flat();
      return [...tables];
    },
    {
      enabled: !isFetching,
    }
  );
};
