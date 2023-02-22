import React, { useEffect } from 'react';
import { useConsoleForm } from '../../../new-components/Form';
import { Button } from '../../../new-components/Button';
import { IndicatorCard } from '../../../new-components/IndicatorCard';
import {
  MetadataSelector,
  useMetadata,
  useRoles,
  useSupportedQueryTypes,
} from '../../MetadataAPI';

import { PermissionsSchema, schema } from './../schema';
import { AccessType, QueryType } from '../types';
import {
  AggregationSection,
  BackendOnlySection,
  ClonePermissionsSection,
  ColumnPermissionsSection,
  ColumnPresetsSection,
  RowPermissionsSection,
  RowPermissionsSectionWrapper,
} from './components';

import { ReturnValue, useFormData, useUpdatePermissions } from './hooks';
import ColumnRootFieldPermissions from './components/RootFieldPermissions/RootFieldPermissions';
import { useListAllTableColumns } from '../../Data';
import { useMetadataSource } from '../../MetadataAPI';
import { omit } from 'lodash';

export interface ComponentProps {
  dataSourceName: string;
  table: unknown;
  queryType: QueryType;
  roleName: string;
  accessType: AccessType;
  handleClose: () => void;
  data: ReturnType<typeof useFormData>['data'];
}

const getCanSave = (
  defaultValues: ReturnValue['defaultValues'],
  newValues: Record<string, any>
) => {
  const cloneWithoutClonePermissions = omit(newValues, 'clonePermissions');
  return (
    JSON.stringify(cloneWithoutClonePermissions) ===
    JSON.stringify(defaultValues)
  );
};

const Component = (props: ComponentProps) => {
  const {
    dataSourceName,
    table,
    queryType,
    roleName,
    accessType,
    handleClose,
    data,
  } = props;

  const { data: metadataTables } = useMetadata(
    MetadataSelector.getTables(dataSourceName)
  );
  const tables = metadataTables?.map(t => t.table) ?? [];
  // functions fired when the form is submitted
  const { updatePermissions, deletePermissions } = useUpdatePermissions({
    dataSourceName,
    table,
    tables,
    queryType,
    roleName,
    accessType,
  });
  const { data: roles } = useRoles();
  const { data: supportedQueryTypes } = useSupportedQueryTypes({
    dataSourceName,
    table,
  });

  const onSubmit = async (formData: PermissionsSchema) => {
    await updatePermissions.submit(formData);
    handleClose();
  };

  const handleDelete = async () => {
    await deletePermissions.submit([queryType]);
    handleClose();
  };

  // for update it is possible to set pre update and post update row checks
  const rowPermissions = queryType === 'update' ? ['pre', 'post'] : [queryType];

  const { formData, defaultValues } = data || {};
  const {
    methods: { getValues, reset },
    Form,
  } = useConsoleForm({
    schema,
    options: {
      defaultValues,
    },
  });

  // Reset form when default values change
  // E.g. when switching tables
  useEffect(() => {
    const newValues = getValues();
    reset({ ...newValues, ...defaultValues });
  }, [roleName, defaultValues]);

  // allRowChecks relates to other queries and is for duplicating from others
  const allRowChecks = defaultValues?.allRowChecks;

  const key = `${JSON.stringify(table)}-${queryType}-${roleName}`;

  const filterType = getValues('filterType');

  return (
    <Form onSubmit={onSubmit} key={key}>
      <div className="bg-white rounded p-md border border-gray-300">
        <div className="pb-4 flex items-center gap-4">
          <Button type="button" onClick={handleClose}>
            Close
          </Button>
          <h3 data-testid="form-title">
            <strong>Role:</strong> {roleName} <strong>Action:</strong>{' '}
            {queryType}
          </h3>
        </div>
        <RowPermissionsSectionWrapper
          roleName={roleName}
          queryType={queryType}
          defaultOpen
        >
          {rowPermissions.map(permissionName => (
            <React.Fragment key={permissionName}>
              {queryType === 'update' && (
                <p className="my-2">
                  <strong>
                    {permissionName === 'pre' ? 'Pre-update' : 'Post-update'}
                    &nbsp; check
                  </strong>
                  &nbsp;
                  {permissionName === 'Post-update' && '(optional)'}
                </p>
              )}
              <RowPermissionsSection
                table={table}
                queryType={queryType}
                subQueryType={
                  queryType === 'update' ? permissionName : undefined
                }
                allRowChecks={allRowChecks || []}
                dataSourceName={dataSourceName}
                supportedOperators={
                  data?.defaultValues?.supportedOperators ?? []
                }
                defaultValues={defaultValues}
              />
            </React.Fragment>
          ))}
        </RowPermissionsSectionWrapper>
        {queryType !== 'delete' && (
          <ColumnPermissionsSection
            roleName={roleName}
            queryType={queryType}
            columns={formData?.columns}
            table={table}
            dataSourceName={dataSourceName}
          />
        )}
        {['insert', 'update'].includes(queryType) && (
          <ColumnPresetsSection
            queryType={queryType}
            columns={formData?.columns}
          />
        )}
        {queryType === 'select' && (
          <AggregationSection queryType={queryType} roleName={roleName} />
        )}
        {['insert', 'update', 'delete'].includes(queryType) && (
          <BackendOnlySection queryType={queryType} />
        )}

        {queryType === 'select' && (
          <ColumnRootFieldPermissions
            filterType={filterType}
            dataSourceName={dataSourceName}
            table={table}
          />
        )}

        <hr className="my-4" />

        <ClonePermissionsSection
          queryType={queryType}
          supportedQueryTypes={supportedQueryTypes}
          tables={tables}
          roles={roles}
        />
        <div className="pt-2 flex gap-2">
          <Button
            type="submit"
            mode="primary"
            title={
              filterType === 'none'
                ? 'You must select an option for row permissions'
                : 'Submit'
            }
            disabled={getCanSave(defaultValues, getValues())}
            isLoading={updatePermissions.isLoading}
          >
            Save Permissions
          </Button>

          <Button
            type="button"
            disabled={accessType === 'noAccess'}
            mode="destructive"
            isLoading={deletePermissions.isLoading}
            onClick={handleDelete}
          >
            Delete Permissions
          </Button>
        </div>
      </div>
    </Form>
  );
};

export interface PermissionsFormProps {
  dataSourceName: string;
  table: unknown;
  queryType: QueryType;
  roleName: string;
  accessType: AccessType;
  handleClose: () => void;
}

// necessary to wrap in this component as otherwise default values are not set properly in useConsoleForm
export const PermissionsForm = (props: PermissionsFormProps) => {
  const { dataSourceName, table, queryType, roleName } = props;

  const { columns: tableColumns, isLoading: isLoadingTables } =
    useListAllTableColumns(dataSourceName, table);

  const { data: metadataSource } = useMetadataSource(dataSourceName);

  const { data, isError, isLoading } = useFormData({
    dataSourceName,
    table,
    queryType,
    roleName,
    tableColumns,
    metadataSource,
  });

  if (isError) {
    return (
      <IndicatorCard status="negative">Error fetching form data</IndicatorCard>
    );
  }

  if (
    isLoading ||
    !data ||
    isLoadingTables ||
    !tableColumns ||
    tableColumns?.length === 0 ||
    !metadataSource ||
    !data.defaultValues
  ) {
    return <IndicatorCard status="info">Loading...</IndicatorCard>;
  }

  return <Component data={data} {...props} />;
};
