import {
  BulkAtomicResponse,
  BulkKeepGoingResponse,
  Table,
} from '../../../hasura-metadata-types';
import { Button } from '../../../../new-components/Button';
import {
  InputField,
  Select,
  useConsoleForm,
} from '../../../../new-components/Form';
import { useEffect } from 'react';
import { Controller } from 'react-hook-form';
import { FaArrowRight, FaLink } from 'react-icons/fa';
import { useRemoteSchemaIntrospection } from '../../hooks/useRemoteSchema';
import { useTableColumns } from '../../hooks/useTableColumns';
import { MODE, Relationship } from '../../types';
import { MapRemoteSchemaFields } from './parts/MapRemoteSchemaFields';
import { MapColumns } from './parts/MapColumns';
import { schema, Schema } from './schema';
import { SourceSelect } from './parts/SourceSelect';
import { useHandleSubmit } from './utils';
import { useSourceOptions } from '../../hooks/useSourceOptions';
import Skeleton from 'react-loading-skeleton';
import { useInconsistentMetadata } from '../../../hasura-metadata-api';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';

interface WidgetProps {
  dataSourceName: string;
  table: Table;
  onCancel: () => void;
  onSuccess: (data: BulkAtomicResponse | BulkKeepGoingResponse) => void;
  onError: (err: Error) => void;
  defaultValue?: Relationship;
}

const getDefaultValue = ({
  dataSourceName,
  table,
  relationship,
}: {
  dataSourceName: string;
  table: Table;
  relationship: Relationship;
}): Schema => {
  if (relationship.type === 'remoteSchemaRelationship')
    return {
      name: relationship.name,
      fromSource: {
        type: 'table',
        dataSourceName,
        table,
      },
      toSource: {
        remoteSchema: relationship.definition.toRemoteSchema,
        type: 'remoteSchema',
      },
      details: {
        rsFieldMapping: relationship.definition.remote_field,
      },
    };

  return {
    name: relationship.name,
    fromSource: {
      type: 'table',
      dataSourceName,
      table,
    },
    toSource: {
      dataSourceName:
        relationship.type === 'remoteDatabaseRelationship'
          ? relationship.definition.toSource
          : dataSourceName,
      table: relationship.definition.toTable,
      type: 'table',
    },
    details: {
      columnMap: Object.entries(relationship.definition.mapping).map(
        ([key, value]) => ({ from: key, to: value })
      ),
      relationshipType: relationship.relationshipType,
    },
  };
};

export const Widget = (props: WidgetProps) => {
  const { dataSourceName, table, onCancel, onSuccess, onError, defaultValue } =
    props;

  const isEditMode = !!defaultValue;

  const { data: sourceOptions } = useSourceOptions();
  const { data: inconsistentSources = [] } = useInconsistentMetadata(m => {
    return m.inconsistent_objects
      .filter(item => item.type === 'source')
      .map(source => source.definition);
  });

  const {
    Form,
    methods: { watch, control, setValue },
  } = useConsoleForm({
    schema,
    options: {
      defaultValues: defaultValue
        ? getDefaultValue({
            dataSourceName,
            table,
            relationship: defaultValue,
          })
        : {
            fromSource: {
              type: 'table',
              dataSourceName,
              table,
            },
          },
    },
  });

  const { data: sourceTableColumns } = useTableColumns({
    dataSourceName,
    table,
  });

  const toSource = watch('toSource');

  const { data: targetTableColumns, isLoading: isColumnDataLoading } =
    useTableColumns({
      dataSourceName: toSource?.type === 'table' ? toSource.dataSourceName : '',
      table: toSource?.type === 'table' ? toSource.table : '',
    });

  const {
    data: remoteSchemaGraphQLSchema,
    isLoading: isRemoteSchemaIntrospectionLoading,
  } = useRemoteSchemaIntrospection({
    remoteSchemaName:
      toSource?.type === 'remoteSchema' ? toSource.remoteSchema : '',
    enabled: toSource?.type === 'remoteSchema',
  });

  const { handleSubmit, ...rest } = useHandleSubmit({
    dataSourceName,
    table,
    mode: !defaultValue ? MODE.CREATE : MODE.EDIT,
    onSuccess,
    onError,
  });

  useEffect(() => {
    if (!defaultValue) {
      if (toSource?.type === 'table') {
        setValue('details.relationshipType', 'Object');
        setValue('details.columnMap', [{ from: '', to: '' }]);
      } else setValue('details', {});
    }
  }, [defaultValue, setValue, toSource]);

  if (inconsistentSources.includes(dataSourceName)) {
    return (
      <IndicatorCard
        headline="Source is inconsistent"
        status="negative"
        showIcon
      >
        Relationships cannot be added to a source that is inconsistent. More
        details are available in Settings tab {'>'} Metadata Status.
      </IndicatorCard>
    );
  }

  return (
    <Form onSubmit={handleSubmit}>
      <div
        id="create-local-rel"
        className="mt-4 px-7"
        style={{ minHeight: '450px' }}
      >
        <InputField
          name="name"
          label="Relationship Name"
          placeholder="Name..."
          dataTest="local-db-to-db-rel-name"
          disabled={isEditMode}
        />

        <div>
          <div className="grid grid-cols-12">
            <div className="col-span-5">
              <SourceSelect
                options={sourceOptions ?? []}
                name="fromSource"
                label="From Source"
                disabled
              />
            </div>

            <div className="col-span-2 flex relative items-center justify-center w-full py-2 mt-3 text-muted">
              <FaArrowRight />
            </div>

            <div className="col-span-5">
              <SourceSelect
                options={sourceOptions ?? []}
                name="toSource"
                label="To Reference"
              />
            </div>
            {inconsistentSources.length ? (
              <div className="col-span-12">
                <IndicatorCard status="negative" showIcon>
                  Inconsistent sources have been found in your metadata.
                  Inconsistent objects will be filtered off from the list of
                  available options until they are fixed.
                </IndicatorCard>
              </div>
            ) : null}
          </div>

          {toSource ? (
            <div className="bg-white rounded-md shadow-sm border border-gray-300 mt-2 mb-4">
              <div className="p-3 text-slate-900 font-semibold text-lg border-b border-gray-300">
                Relationship Details
              </div>
              {isRemoteSchemaIntrospectionLoading || isColumnDataLoading ? (
                <div className="px-sm m-sm">
                  <Skeleton height={30} count={5} className="my-2" />
                </div>
              ) : (
                <div className="px-6 pt-4">
                  {toSource?.type === 'table' && (
                    <div>
                      <div className="px-6 pt-4 w-1/3">
                        <Select
                          name="details.relationshipType"
                          label="Relationship Type"
                          dataTest="local-db-to-db-select-rel-type"
                          placeholder="Select a relationship type..."
                          options={[
                            {
                              label: 'Object Relationship',
                              value: 'Object',
                            },
                            {
                              label: 'Array Relationship',
                              value: 'Array',
                            },
                          ]}
                        />
                      </div>

                      <MapColumns
                        name="details.columnMap"
                        targetTableColumns={targetTableColumns ?? []}
                        sourceTableColumns={sourceTableColumns ?? []}
                      />
                    </div>
                  )}
                  {toSource?.type === 'remoteSchema' &&
                    remoteSchemaGraphQLSchema &&
                    sourceTableColumns && (
                      <div>
                        <Controller
                          control={control}
                          name="details.rsFieldMapping"
                          render={({ field: { onChange, value } }) => (
                            <MapRemoteSchemaFields
                              graphQLSchema={remoteSchemaGraphQLSchema}
                              onChange={onChange}
                              defaultValue={value}
                              tableColumns={sourceTableColumns.map(
                                col => col.name
                              )}
                            />
                          )}
                        />
                      </div>
                    )}
                </div>
              )}
            </div>
          ) : (
            <div
              style={{ minHeight: '200px' }}
              className="bg-gray-100 rounded-md shadow-sm border border-gray-300 mt-2 mb-4 h-20 flex items-center justify-center flex-col"
            >
              <div>
                <FaLink />
              </div>
              <div>
                Please select a source and a reference to create a relationship
              </div>
            </div>
          )}
        </div>
      </div>
      <div className="flex justify-end gap-2 sticky bottom-0 bg-slate-50 px-8 py-3 border-t border-gray-300">
        <Button onClick={onCancel}>Close</Button>
        <Button
          type="submit"
          mode="primary"
          isLoading={rest.isLoading}
          loadingText="Creating"
        >
          {isEditMode ? 'Edit Relationship' : 'Create Relationship'}
        </Button>
      </div>
    </Form>
  );
};
