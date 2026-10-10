import { useFormContext } from 'react-hook-form';
import {
  IndicatorCard,
  LinkBlockVertical,
  LinkBlockHorizontal,
  SkeletonList,
} from '@hasura/shared/ui';

import { RemoteSchemaWidget } from '../RemoteSchemaWidget';
import { RsSourceTypeSelector } from '../RsSourceTypeSelector';
import {
  refRemoteOperationSelectorKey,
  refRemoteSchemaSelectorKey,
  RefRsSelector,
} from '../RefRsSelector';
import {
  getFieldTypesFromType,
  getTypesFromIntrospection,
} from '../../../utils';
import {
  useIntrospectRemoteSchema,
  useListRemoteSchemas,
} from '@hasura/metadata/api';
import { Flex } from '@radix-ui/themes';
import { RemoteRelationship } from '@hasura/shared/types';
import { RsToRsSchema } from './schemas';

export type RemoteSchemaToRemoteSchemaFormProps = {
  sourceRemoteSchema: string;
  existingRelationship?: RemoteRelationship;
};

const rsSourceTypeKey = 'rsSourceType';

const useLoadData = (sourceRemoteSchema: string) => {
  const {
    data: remoteSchemaList,
    isLoading: listLoading,
    isError: listError,
  } = useListRemoteSchemas();
  const {
    data: rsData,
    isLoading: schemaLoading,
    isError: schemaError,
  } = useIntrospectRemoteSchema(sourceRemoteSchema);
  const { watch } = useFormContext<RsToRsSchema>();
  const refRemoteSchemaName = watch(refRemoteSchemaSelectorKey);
  const rsSourceType = watch(rsSourceTypeKey);
  const selectedOperation = watch(refRemoteOperationSelectorKey);
  const remoteSchemaTypes = (rsData && getTypesFromIntrospection(rsData)) ?? [];

  const fieldsForSelectedRsType = getFieldTypesFromType(
    remoteSchemaTypes,
    rsSourceType,
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
  existingRelationship,
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

  if (isLoading && !isError) {
    return (
      <div className="my-2">
        <SkeletonList count={5} />
      </div>
    );
  }

  if (isError || !remoteSchemaList || !sourceRemoteSchema) {
    return (
      <div className="my-2">
        <IndicatorCard status="negative" showIcon>
          Error loading remote schemas
        </IndicatorCard>
      </div>
    );
  }

  return (
    <>
      <div className="grid grid-cols-12 mt-4">
        <div className="col-span-5">
          <RsSourceTypeSelector
            remoteSchemaName={sourceRemoteSchema}
            types={remoteSchemaTypes.map((t) => t.typeName).sort()}
            sourceTypeKey={rsSourceTypeKey}
            nameTypeKey="name"
            isModify={!!existingRelationship}
          />
        </div>
        <Flex className="col-span-2" align="center">
          <LinkBlockHorizontal />
        </Flex>
        {/* select the reference remote schema */}
        <div className="col-span-5">
          <RefRsSelector allRemoteSchemas={remoteSchemaList} />
        </div>
      </div>

      <div className={selectedOperation ? '' : 'hidden mb-2'}>
        <LinkBlockVertical title="Refine Relationship Mapping" />

        {/* relationship details */}
        <div className="grid w-full pb-4">
          <RemoteSchemaWidget
            showOnlySelectable={true}
            schemaName={refRemoteSchemaName}
            fields={fieldsForSelectedRsType}
            serverRelationship={existingRelationship}
            rootFields={['query']}
          />
        </div>
      </div>
    </>
  );
};
