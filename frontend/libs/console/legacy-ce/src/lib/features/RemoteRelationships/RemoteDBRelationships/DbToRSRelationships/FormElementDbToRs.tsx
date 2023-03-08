import React, { useEffect } from 'react';
import { useFormContext } from 'react-hook-form';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import {
  LinkBlockVertical,
  LinkBlockHorizontal,
} from '../../../../new-components/LinkBlock';
import { useListRemoteSchemas } from '../../../MetadataAPI';
import { useTableColumns } from '../../../SqlQueries';
import { DatabaseSelector } from '../../../Data/components/index';
import { DataTarget } from '../../../Datasources';

import { RemoteSchemaWidget } from '../../RemoteSchemaRelationships/components/RemoteSchemaWidget';
import {
  refRemoteSchemaSelectorKey,
  RefRsSelector,
} from '../../RemoteSchemaRelationships/components/RefRsSelector';
import { DbToRsSchema } from './schema';
import {
  RemoteRelationship,
  RsToRsSchema,
} from '../../RemoteSchemaRelationships/types';

type FormElementsDbToRsProps = {
  sourceTableInfo: DataTarget;
  selectedRelationship?: RemoteRelationship;
};

const useLoadData = () => {
  const {
    data: remoteSchemaList,
    isLoading: listLoading,
    isError: listError,
  } = useListRemoteSchemas();

  const { watch } = useFormContext<DbToRsSchema>();
  const refRemoteSchemaName = watch(refRemoteSchemaSelectorKey);

  const isLoading = listLoading;

  const isError = listError;
  return {
    data: {
      refRemoteSchemaName,
      remoteSchemaList,
    },
    isLoading,
    isError,
  };
};

export const FormElementDbToRs = ({
  sourceTableInfo,
  selectedRelationship,
}: FormElementsDbToRsProps) => {
  const sourceDB = sourceTableInfo.database;
  const sourceTable = sourceTableInfo.table;

  let sourceSchema;
  if ('schema' in sourceTableInfo) sourceSchema = sourceTableInfo?.schema;

  const {
    data: { remoteSchemaList, refRemoteSchemaName },
    isLoading,
    isError,
  } = useLoadData();

  const { data: sourceColumnData } = useTableColumns(sourceDB, {
    name: sourceTable,
    schema: sourceSchema ?? '',
  });
  const { reset } = useFormContext<RsToRsSchema>();

  useEffect(() => {
    if (!selectedRelationship) return;

    const defaultValues = {
      resultSet:
        selectedRelationship?.definition?.to_remote_schema.remote_field ?? {},
      relationshipName: selectedRelationship?.name ?? '',
      [refRemoteSchemaSelectorKey]:
        selectedRelationship?.definition?.to_remote_schema?.remote_schema ?? '',
      relationship: selectedRelationship,
    };
    reset(defaultValues);
  }, [selectedRelationship, reset]);

  if (isLoading && !isError) {
    return <IndicatorCard status="info">Loading...</IndicatorCard>;
  }
  if (isError) {
    return (
      <IndicatorCard status="negative">
        Error loading remote schemas
      </IndicatorCard>
    );
  }
  if (!remoteSchemaList) {
    return <IndicatorCard status="info">No remote schemas found</IndicatorCard>;
  }
  return (
    <>
      <div className="grid grid-cols-12">
        <div className="col-span-5">
          <DatabaseSelector
            value={sourceTableInfo}
            name="source"
            className="border-l-4 border-l-green-600"
            disabledKeys={['database', 'schema', 'table']}
          />
        </div>
        <LinkBlockHorizontal />
        <div className="col-span-5">
          <RefRsSelector allRemoteSchemas={remoteSchemaList} />
        </div>
      </div>
      <LinkBlockVertical title="Related To" />
      {/* relationship details */}
      <div className="grid w-full pb-md">
        <RemoteSchemaWidget
          schemaName={refRemoteSchemaName}
          fields={
            sourceColumnData
              ? sourceColumnData?.slice(1).map((x: string[]) => x[3])
              : []
          }
          rootFields={['query', 'mutation']}
        />
      </div>
    </>
  );
};
