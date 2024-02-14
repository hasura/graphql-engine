import React from 'react';
import { useFormContext } from 'react-hook-form';

import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import {
  LinkBlockVertical,
  LinkBlockHorizontal,
} from '../../../../../new-components/LinkBlock';

import { useListRemoteSchemas, useRemoteSchema } from '../../../../MetadataAPI';

import { RemoteSchemaWidget } from '../RemoteSchemaWidget';
import { RsSourceTypeSelector } from '../RsSourceTypeSelector';
import {
  refRemoteOperationSelectorKey,
  refRemoteSchemaSelectorKey,
  RefRsSelector,
} from '../RefRsSelector';
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
  const selectedOperation = watch(refRemoteOperationSelectorKey);

  React.useEffect(() => {
    if (sourceRemoteSchema) {
      fetchSchema(sourceRemoteSchema);
    }
  }, [fetchSchema, sourceRemoteSchema]);

  const remoteSchemaList = React.useMemo(() => {
    return data;
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
      selectedOperation,
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
      selectedOperation,
    },
    isLoading,
    isError,
  } = useLoadData(sourceRemoteSchema);

  const ref = React.useRef<HTMLDivElement>(null);

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
      <div className="grid grid-cols-12 my-md">
        <div className="col-span-5">
          <RsSourceTypeSelector
            types={remoteSchemaTypes.map(t => t.typeName).sort()}
            sourceTypeKey={rsSourceTypeKey}
            nameTypeKey="name"
            isModify={!!existingRelationshipName}
          />
        </div>

        <LinkBlockHorizontal />

        {/* select the reference remote schema */}
        <div className="col-span-5">
          <RefRsSelector allRemoteSchemas={remoteSchemaList} />
        </div>
      </div>

      <div className={selectedOperation ? '' : 'hidden mb-2'}>
        <LinkBlockVertical title="Refine Relationship Mapping" />

        {/* relationship details */}
        <div ref={ref} className="grid w-full pb-md">
          <RemoteSchemaWidget
            showOnlySelectable={true}
            schemaName={refRemoteSchemaName}
            fields={fieldsForSelectedRsType}
            rootFields={['query']}
          />
        </div>
      </div>
    </>
  );
};
