import React from 'react';
import { useFormContext } from 'react-hook-form';

import { InputField } from '@/new-components/Form';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import {
  LinkBlockVertical,
  LinkBlockHorizontal,
} from '@/new-components/LinkBlock';

import { useListRemoteSchemas, useRemoteSchema } from '@/features/MetadataAPI';

import { RemoteSchemaWidget } from '../RemoteSchemaWidget';
import { RsSourceTypeSelector } from '../RsSourceTypeSelector';
import { refRemoteSchemaSelectorKey, RefRsSelector } from '../RefRsSelector';
import { getFieldTypesFromType, getTypesFromIntrospection } from '../../utils';
import { RsToRsSchema } from '../../types';

export type RemoteSchemaToRemoteSchemaFormProps = {
  sourceRemoteSchema: string;
  existingRelationshipName?: string;
};

const rsSourceTypeKey = 'rsSourceType';

const useLoadData = (sourceRemoteSchema: string) => {
  const {
    data,
    isLoading: listLoading,
    isError: listError,
  } = useListRemoteSchemas();
  const {
    fetchSchema,
    data: rsData,
    isLoading: schemaLoading,
    isError: schemaError,
  } = useRemoteSchema();
  const { watch } = useFormContext<RsToRsSchema>();
  const refRemoteSchemaName = watch(refRemoteSchemaSelectorKey);
  const rsSourceType = watch(rsSourceTypeKey);

  React.useEffect(() => {
    if (sourceRemoteSchema) {
      fetchSchema(sourceRemoteSchema);
    }
  }, [fetchSchema, sourceRemoteSchema]);

  const remoteSchemaList = React.useMemo(() => {
    return data?.filter(schemaName => schemaName !== sourceRemoteSchema);
  }, [data, sourceRemoteSchema]);

  const remoteSchemaTypes = (rsData && getTypesFromIntrospection(rsData)) ?? [];

  const fieldsForSelectedRsType = getFieldTypesFromType(
    remoteSchemaTypes,
    rsSourceType
  );

  const isLoading =
    listLoading ||
    schemaLoading ||
    !remoteSchemaTypes?.length ||
    !fieldsForSelectedRsType;

  const isError = listError || schemaError;

  return {
    data: {
      remoteSchemaList,
      refRemoteSchemaName,
      remoteSchemaTypes,
      fieldsForSelectedRsType,
    },
    isLoading,
    isError,
  };
};

export const FormElements = ({
  sourceRemoteSchema,
  existingRelationshipName,
}: RemoteSchemaToRemoteSchemaFormProps) => {
  const {
    data: {
      remoteSchemaList,
      refRemoteSchemaName,
      fieldsForSelectedRsType,
      remoteSchemaTypes,
    },
    isLoading,
    isError,
  } = useLoadData(sourceRemoteSchema);

  if (isLoading && !isError) {
    return (
      <div className="my-2">
        <IndicatorCard status="info">Loading...</IndicatorCard>
      </div>
    );
  }

  if (isError || !remoteSchemaList || !sourceRemoteSchema) {
    return (
      <div className="my-2">
        <IndicatorCard status="negative">
          Error loading remote schemas
        </IndicatorCard>
      </div>
    );
  }

  return (
    <>
      <div className="w-full sm:w-6/12 my-md">
        <InputField
          name="name"
          label="Name"
          placeholder="Relationship name"
          dataTest="rs-to-rs-rel-name"
          disabled={!!existingRelationshipName}
        />
      </div>

      <div className="grid grid-cols-12">
        <div className="col-span-5">
          <RsSourceTypeSelector
            types={remoteSchemaTypes.map(t => t.typeName).sort()}
            sourceTypeKey={rsSourceTypeKey}
          />
        </div>

        <LinkBlockHorizontal />

        {/* select the reference remote schema */}
        <div className="col-span-5">
          <RefRsSelector allRemoteSchemas={remoteSchemaList} />
        </div>
      </div>

      <LinkBlockVertical title="Type Mapped To" />

      {/* relationship details */}
      <div className="grid w-full pb-md">
        <RemoteSchemaWidget
          schemaName={refRemoteSchemaName}
          fields={fieldsForSelectedRsType}
          rootFields={['query', 'mutation']}
        />
      </div>
    </>
  );
};
