import {
  BulkAtomicResponse,
  BulkKeepGoingResponse,
  SupportedDriver,
  Table,
} from '@hasura/shared/types';
import {
  Button,
  InputField,
  useConsoleForm,
  IndicatorCard,
  SkeletonList,
  SelectField,
  Text,
  Card,
  ReactSelectField,
  getDialogPortalTarget,
} from '@hasura/shared/ui';
import { useEffect } from 'react';
import { Flex } from '@radix-ui/themes';
import { Controller } from 'react-hook-form';
import { FaArrowRight, FaLink } from 'react-icons/fa';
import { MODE, Relationship } from '../../types';
import { MapRemoteSchemaFields } from './parts/MapRemoteSchemaFields';
import { MapColumns } from './parts/MapColumns';
import { schema, Schema } from './schema';
import { SourceOption } from './parts/SourceSelect';
import { useHandleSubmit } from './utils';
import { useIntrospectRemoteSchema } from '@hasura/metadata/api';
import { useTableColumns } from '@hasura/metadata/data-source';
import { getTableLabel } from '@hasura/shared/utils';
import { createFilter } from 'react-select';

interface WidgetProps {
  dataSourceName: string;
  table: Table;
  onCancel: () => void;
  onSuccess: (data: BulkAtomicResponse | BulkKeepGoingResponse) => void;
  onError: (err: Error) => void;
  defaultValue?: Relationship;
  sourceOptions: SourceOption[];
  inconsistentSources: string[];
}

const DATABASE_RELATIONSHIP_OPTIONS = [
  {
    label: 'Object Relationship',
    value: 'Object',
  },
  {
    label: 'Array Relationship',
    value: 'Array',
  },
];
const adaptDataSourceKind = (
  options: SourceOption[] | undefined,
  dataSourceName: string,
): SupportedDriver | undefined => {
  if (!options) {
    return undefined;
  }

  for (const option of options) {
    if (option.value.type !== 'table') {
      continue;
    }

    if (option.value.dataSourceName === dataSourceName) {
      return option.value.driver;
    }
  }

  return undefined;
};

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
        ([key, value]) => ({ from: key, to: value }),
      ),
      relationshipType: relationship.relationshipType,
    },
  };
};

export const Widget = ({
  dataSourceName,
  table,
  onCancel,
  onSuccess,
  onError,
  defaultValue,
  sourceOptions,
  inconsistentSources,
}: WidgetProps) => {
  const isEditMode = !!defaultValue;

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
    source: {
      name: dataSourceName,
      kind: adaptDataSourceKind(sourceOptions, dataSourceName)!,
    },
    table,
  });

  const toSource = watch('toSource');

  const { data: targetTableColumns, isLoading: isColumnDataLoading } =
    useTableColumns({
      source:
        toSource?.type === 'table'
          ? {
              name: toSource.dataSourceName,
              kind: adaptDataSourceKind(
                sourceOptions,
                toSource.dataSourceName,
              )!,
            }
          : undefined,
      table: toSource?.type === 'table' ? toSource.table : '',
    });

  const {
    data: remoteSchemaGraphQLSchema,
    isLoading: isRemoteSchemaIntrospectionLoading,
  } = useIntrospectRemoteSchema(
    toSource?.type === 'remoteSchema' ? toSource.remoteSchema : '',
    {
      enabled: toSource?.type === 'remoteSchema',
    },
  );

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
      } else setValue('details', { rsFieldMapping: undefined });
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
        className="mt-4"
        style={{ minHeight: '450px' }}
      >
        <InputField
          name="name"
          label="Relationship Name"
          dataTest="local-db-to-db-rel-name"
          fieldProps={{
            placeholder: 'Name...',
            disabled: isEditMode,
          }}
        />

        <div>
          <div className="grid grid-cols-12">
            <div className="col-span-5">
              <SelectField
                options={[]}
                name="fromSource"
                label="From Source"
                placeholder={getTableLabel({
                  dataSourceName: dataSourceName,
                  table,
                })}
                disabled
              />
            </div>

            <Flex
              align="center"
              justify="center"
              className="col-span-2 relative w-full py-2 mt-3 text-muted"
            >
              <FaArrowRight />
            </Flex>

            <div className="col-span-5">
              <ReactSelectField
                options={sourceOptions ?? []}
                name="toSource"
                label="To Reference"
                selectProps={{
                  isSearchable: true,
                  filterOption: createFilter({
                    ignoreCase: true,
                    matchFrom: 'any',
                  }),
                  menuPortalTarget: getDialogPortalTarget(),
                }}
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
            <Card size="3">
              <Text size="3" weight="bold">
                Relationship Details
              </Text>
              {isRemoteSchemaIntrospectionLoading || isColumnDataLoading ? (
                <div className="my-2">
                  <SkeletonList count={5} />
                </div>
              ) : (
                <div>
                  {toSource?.type === 'table' && (
                    <div>
                      <div className="pt-4 w-1/3">
                        <SelectField
                          name="details.relationshipType"
                          label="Relationship Type"
                          dataTest="local-db-to-db-select-rel-type"
                          placeholder="Select a relationship type..."
                          options={DATABASE_RELATIONSHIP_OPTIONS}
                        />
                      </div>

                      <MapColumns
                        name="details.columnMap"
                        targetTableColumns={targetTableColumns?.columns ?? []}
                        sourceTableColumns={sourceTableColumns?.columns ?? []}
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
                              tableColumns={sourceTableColumns.columns.map(
                                (col) => col.name,
                              )}
                            />
                          )}
                        />
                      </div>
                    )}
                </div>
              )}
            </Card>
          ) : (
            <Flex
              direction="column"
              align="center"
              justify="center"
              style={{ minHeight: '200px' }}
            >
              <FaLink />
              <Text>
                Please select a source and a reference to create a relationship
              </Text>
            </Flex>
          )}
        </div>
      </div>
      <Flex justify="end" gap="2" className="mt-4">
        <Button mode="default" onClick={onCancel}>
          Close
        </Button>
        <Button
          type="submit"
          mode="primary"
          disabled={rest.isPending}
          loadingText="Creating"
        >
          {isEditMode ? 'Edit Relationship' : 'Create Relationship'}
        </Button>
      </Flex>
    </Form>
  );
};
