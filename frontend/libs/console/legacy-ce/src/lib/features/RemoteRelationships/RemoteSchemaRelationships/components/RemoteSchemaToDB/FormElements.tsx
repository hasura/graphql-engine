import React, { useEffect, useState } from 'react';
import { useFormContext } from 'react-hook-form';
import { useRemoteSchema } from '../../../../MetadataAPI';
import { MapSelector } from '../../../../../new-components/MapSelector';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import {
  LinkBlockHorizontal,
  LinkBlockVertical,
} from '../../../../../new-components/LinkBlock';
import { RemoteDatabaseWidget } from '../RemoteDatabaseWidget';
import { RsSourceTypeSelector } from '../RsSourceTypeSelector';
import { Schema } from './schema';
import { getTypesFromIntrospection } from '../../utils';
import { useTableColumns } from '../../../../DatabaseRelationships/hooks/useTableColumns';

export const FormElements = ({
  sourceRemoteSchema,
  existingRelationshipName,
}: {
  sourceRemoteSchema: string;
  existingRelationshipName: string;
}) => {
  const { watch } = useFormContext<Schema>();

  const target = watch('target');
  const RSTypeName = watch('typeName');
  const mapping = watch('mapping');
  const { fetchSchema, data, isLoading } = useRemoteSchema();

  const [typeMap, setTypeMap] = useState<{ field: string; column: string }[]>(
    []
  );

  const dataSourceName = target?.dataSourceName;
  const table = target?.table;
  const { data: columnData } = useTableColumns({ dataSourceName, table });

  const columns: string[] = columnData
    ? (columnData ?? []).map(column => column.name)
    : [];

  useEffect(() => {
    if (sourceRemoteSchema) {
      fetchSchema(sourceRemoteSchema);
    }
  }, [fetchSchema, sourceRemoteSchema]);

  useEffect(() => {
    const defaultMapping = mapping?.length ? mapping : [];
    if (existingRelationshipName && mapping?.length) setTypeMap(defaultMapping);
  }, [RSTypeName, existingRelationshipName, mapping]);

  if (isLoading) {
    return (
      <div className="my-2">
        <IndicatorCard status="info">Loading...</IndicatorCard>
      </div>
    );
  }
  if (!data)
    return (
      <div className="my-2">
        <IndicatorCard status="info">Data is not ready</IndicatorCard>;
      </div>
    );

  const remoteSchemaTypes = getTypesFromIntrospection(data);

  return (
    <>
      <div className="grid grid-cols-12 mt-md">
        <div className="col-span-5">
          <RsSourceTypeSelector
            types={remoteSchemaTypes.map(t => t.typeName).sort()}
            sourceTypeKey="typeName"
            nameTypeKey="relationshipName"
            isModify={!!existingRelationshipName}
          />
        </div>

        <LinkBlockHorizontal />

        <div className="col-span-5 pt-10 mt-20">
          <RemoteDatabaseWidget />
        </div>
      </div>

      {/* vertical connector line */}
      <LinkBlockVertical title="Type Mapped To" />
      <MapSelector
        types={
          remoteSchemaTypes.find(x => x.typeName === RSTypeName)?.fields ?? []
        }
        columns={columns}
        typeMappings={typeMap}
        placeholder=""
        name="mapping"
        onChange={e => {
          setTypeMap([...e]);
        }}
      />
    </>
  );
};
