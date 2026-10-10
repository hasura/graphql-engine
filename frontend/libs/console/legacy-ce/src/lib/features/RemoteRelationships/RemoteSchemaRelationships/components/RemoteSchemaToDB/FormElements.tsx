import { useEffect } from 'react';
import { useFormContext } from 'react-hook-form';
import {
  IndicatorCard,
  LinkBlockHorizontal,
  LinkBlockVertical,
  SkeletonList,
} from '@hasura/shared/ui';
import { RemoteDatabaseWidget } from '../RemoteDatabaseWidget';
import { RsSourceTypeSelector } from '../RsSourceTypeSelector';
import { Schema } from './schema';
import { getTypesFromIntrospection } from '../../../utils';
import { useTableColumns } from '@hasura/metadata/data-source';
import { useIntrospectRemoteSchema, useMetadata } from '@hasura/metadata/api';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import { Flex, Skeleton } from '@radix-ui/themes';
import { MapSelector } from './MapSelector';
import { Metadata, RemoteRelationship } from '@hasura/shared/types';

export const FormElements = ({
  sourceRemoteSchema,
  existingRelationship,
}: {
  sourceRemoteSchema: string;
  existingRelationship: RemoteRelationship | undefined;
}) => {
  const { data: meta, isFetching: isFetchingMetadata } = useMetadata();
  const { data, isFetching: isFetchingRemoteSchema } =
    useIntrospectRemoteSchema(sourceRemoteSchema);

  const { watch } = useFormContext<Schema>();
  const target = watch('target');

  if (isFetchingMetadata || isFetchingRemoteSchema) {
    return (
      <div className="my-2">
        <SkeletonList count={5} />
      </div>
    );
  }

  if (!data || !meta)
    return (
      <div className="my-2">
        <IndicatorCard status="info">Data is not ready</IndicatorCard>;
      </div>
    );

  const remoteSchemaTypes = getTypesFromIntrospection(data);

  return (
    <>
      <div className="grid grid-cols-12 mt-4">
        <div className="col-span-5">
          <RsSourceTypeSelector
            types={remoteSchemaTypes.map((t) => t.typeName).sort()}
            sourceTypeKey="typeName"
            nameTypeKey="relationshipName"
            remoteSchemaName={sourceRemoteSchema}
            isModify={!!existingRelationship}
          />
        </div>

        <Flex className="col-span-2 h-full" align="center">
          <LinkBlockHorizontal />
        </Flex>

        <Flex className="col-span-5 h-full" align="center">
          <RemoteDatabaseWidget meta={meta} />
        </Flex>
      </div>

      {Boolean(target.table) && (
        <RelationshipMapSelector
          existingRelationship={existingRelationship}
          remoteSchemaTypes={remoteSchemaTypes}
          meta={meta}
        />
      )}
    </>
  );
};

const RelationshipMapSelector = ({
  meta,
  remoteSchemaTypes,
}: {
  meta: Metadata;
  remoteSchemaTypes: {
    typeName: string;
    fields: string[];
  }[];
  existingRelationship: RemoteRelationship | undefined;
}) => {
  const { watch, setValue } = useFormContext<Schema>();
  const target = watch('target');
  const rsTypeName = watch('typeName');
  const mapping = watch('mapping');

  const table = target?.table;
  const dataSourceName = target?.dataSourceName;
  const source = MetadataSelectors.findSource(dataSourceName)(meta);

  const { data: columnData, isFetching: isFetchingColumns } = useTableColumns({
    source,
    table,
  });

  const columns: string[] = columnData
    ? (columnData.columns ?? []).map((column) => column.name)
    : [];
  const types =
    remoteSchemaTypes.find((x) => x.typeName === rsTypeName)?.fields ?? [];

  useEffect(() => {
    setValue(
      'mapping',
      mapping.filter(
        (typeMap) =>
          columns.includes(typeMap.column) && types.includes(typeMap.field),
      ),
    );
  }, [columns, types]);

  return (
    <>
      {/* vertical connector line */}

      <LinkBlockVertical title="Type Mapped To" />
      {isFetchingColumns ? (
        <Skeleton width="100%" height="100px" />
      ) : (
        <MapSelector
          types={types}
          columns={columns}
          mapping={mapping}
          setMapping={(values) => setValue('mapping', values)}
        />
      )}
    </>
  );
};
