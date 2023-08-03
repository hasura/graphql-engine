import React, { useRef } from 'react';
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

import {
  createDefaultValues,
  useFormData,
  useUpdatePermissions,
} from './hooks';
import ColumnRootFieldPermissions from './components/RootFieldPermissions/RootFieldPermissions';
import { useListAllTableColumns } from '../../Data';
import { useMetadataSource } from '../../MetadataAPI';
import useScrollIntoView from './hooks/useScrollIntoView';
import {
  createFormData,
  getAllowedFilterKeys,
} from './hooks/dataFetchingHooks/useFormData/createFormData/index';
import Skeleton from 'react-loading-skeleton';
import { CommentSection } from './components/CommentSection';

export interface ComponentProps {
  dataSourceName: string;
  table: unknown;
  queryType: QueryType;
  roleName: string;
  accessType: AccessType;
  handleClose: () => void;
  defaultValues: ReturnType<typeof createDefaultValues>;
  formData: ReturnType<typeof createFormData>;
}

const Component = (props: ComponentProps) => {
  const {
    dataSourceName,
    table,
    queryType,
    roleName,
    accessType,
    handleClose,
    defaultValues,
    formData,
  } = props;
  const permissionSectionRef = useRef(null);

  useScrollIntoView(permissionSectionRef, [roleName], { behavior: 'smooth' });

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
    const newValues = getValues();
    try {
      await updatePermissions.submit(formData);
      handleClose();
    } catch (e) {
      reset(newValues);
    }
  };

  const handleDelete = async () => {
    await deletePermissions.submit([queryType]);
    handleClose();
  };

  const {
    methods: { getValues, reset },
    Form,
  } = useConsoleForm({
    schema,
    options: {
      defaultValues,
    },
  });

  const key = `${JSON.stringify(table)}-${queryType}-${roleName}`;

  const filterType = getValues('filterType');
  const filterKeys = getAllowedFilterKeys(queryType);
  return (
    <Form onSubmit={onSubmit} key={key}>
      <div
        className="bg-white rounded p-md border border-gray-300"
        data-testid="permissions-form"
      >
        <div className="pb-4 flex items-center gap-4">
          <Button type="button" onClick={handleClose}>
            Close
          </Button>
          <h3 data-testid="form-title">
            <strong>Role:</strong> {roleName} <strong>Action:</strong>{' '}
            {queryType}
          </h3>
        </div>
        <CommentSection key={key} />
        <RowPermissionsSectionWrapper
          roleName={roleName}
          queryType={queryType}
          defaultOpen
        >
          <React.Fragment key={key}>
            {queryType === 'update' && (
              <p className="my-2">
                <strong>Pre-update &nbsp; check</strong>
                &nbsp;
              </p>
            )}
            <RowPermissionsSection
              table={table}
              roleName={roleName}
              queryType={queryType}
              subQueryType={queryType === 'update' ? 'pre_update' : undefined}
              permissionsKey={filterKeys[0]}
              dataSourceName={dataSourceName}
              supportedOperators={defaultValues?.supportedOperators ?? []}
              defaultValues={defaultValues}
            />
            {queryType === 'update' && (
              <div data-testid="post-update-check-container">
                <p className="my-2">
                  <strong>Post-update &nbsp; check</strong>
                  &nbsp; (optional)
                </p>

                <RowPermissionsSection
                  table={table}
                  roleName={roleName}
                  queryType={queryType}
                  subQueryType={
                    queryType === 'update' ? 'post_update' : undefined
                  }
                  permissionsKey={filterKeys[1]}
                  dataSourceName={dataSourceName}
                  supportedOperators={defaultValues?.supportedOperators ?? []}
                  defaultValues={defaultValues}
                />
              </div>
            )}
          </React.Fragment>
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

        <div
          ref={permissionSectionRef}
          className="pt-2 flex gap-2"
          id="form-buttons-container"
        >
          <Button
            type="submit"
            mode="primary"
            title={
              filterType === 'none'
                ? 'You must select an option for row permissions'
                : 'Submit'
            }
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
  });

  if (!data || !metadataSource) {
    return <Skeleton width={'100%'} height={300} />;
  }

  const defaultValues = createDefaultValues({
    queryType,
    roleName,
    dataSourceName,
    metadata: data?.metadata,
    table,
    tableColumns,
    defaultQueryRoot: data.defaultQueryRoot,
    metadataSource,
    supportedOperators: data.supportedOperators,
  });

  const formData = createFormData({
    dataSourceName,
    table,
    metadata: data.metadata,
    tableColumns,
    trackedTables: metadataSource.tables,
    metadataSource,
  });

  if (isError) {
    return (
      <IndicatorCard status="negative">Error fetching form data</IndicatorCard>
    );
  }

  if (
    isLoading ||
    isLoadingTables ||
    !tableColumns ||
    tableColumns?.length === 0
  ) {
    return <Skeleton width={'100%'} height={300} />;
  }

  return (
    <Component
      // Reset component when defaultValues change
      // Otherwise the form keeps the old values
      // Tried using react-hook-form's `reset` but it's overwritten by old default values (before submitting)
      key={`${dataSourceName}-${JSON.stringify(
        table
      )}-${queryType}-${roleName}-${JSON.stringify(defaultValues)}`}
      defaultValues={defaultValues}
      formData={formData}
      {...props}
    />
  );
};
