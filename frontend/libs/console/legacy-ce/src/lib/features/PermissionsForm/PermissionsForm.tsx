import React from 'react';

import { UpdatedForm as Form } from '@/new-components/Form';
import { Button } from '@/new-components/Button';
import { IndicatorCard } from '@/new-components/IndicatorCard';

import { PermissionsSchema, schema } from './utils/formSchema';

import { AccessType, QueryType } from './types';
import {
  AggregationSection,
  BackendOnlySection,
  ColumnPermissionsSection,
  ColumnPresetsSection,
  RowPermissionsSection,
  RowPermissionsSectionWrapper,
} from './components';

import { useFormData, useUpdatePermissions } from './hooks';

export interface PermissionsFormProps {
  dataSourceName: string;
  table: unknown;
  queryType: QueryType;
  roleName: string;
  accessType: AccessType;
  handleClose: () => void;
}

export const PermissionsForm = (props: PermissionsFormProps) => {
  const {
    dataSourceName,
    table,
    queryType,
    roleName,
    accessType,
    handleClose,
  } = props;

  const { data, isError, isLoading } = useFormData({
    dataSourceName,
    table,
    queryType,
    roleName,
  });

  // functions fired when the form is submitted
  const { updatePermissions, deletePermissions } = useUpdatePermissions({
    dataSourceName,
    table,
    queryType,
    roleName,
    accessType,
  });

  const handleSubmit = async (formData: PermissionsSchema) => {
    await updatePermissions.submit(formData);
    handleClose();
  };

  const handleDelete = async () => {
    await deletePermissions.submit([queryType]);
    handleClose();
  };

  const isSubmittingError =
    updatePermissions.isError || deletePermissions.isError;

  // for update it is possible to set pre update and post update row checks
  const rowPermissions = queryType === 'update' ? ['pre', 'post'] : [queryType];

  if (isSubmittingError) {
    return (
      <IndicatorCard status="negative">Error submitting form</IndicatorCard>
    );
  }

  // these will be replaced by components once spec is decided
  if (isError) {
    return (
      <IndicatorCard status="negative">Error fetching form data</IndicatorCard>
    );
  }

  // these will be replaced by components once spec is decided
  if (isLoading || !data) {
    return <IndicatorCard status="info">Loading...</IndicatorCard>;
  }

  const { formData, defaultValues } = data;

  // allRowChecks relates to other queries and is for duplicating from others
  const allRowChecks = defaultValues?.allRowChecks;

  const key = `${JSON.stringify(table)}-${queryType}-${roleName}`;

  return (
    <Form
      key={key}
      onSubmit={handleSubmit}
      schema={schema}
      options={{ defaultValues }}
    >
      {options => {
        const filterType = options.getValues('filterType');
        if (Object.keys(options.formState.errors)?.length) {
          console.error('form errors:', options.formState.errors);
        }
        return (
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
                        {permissionName === 'pre'
                          ? 'Pre-update'
                          : 'Post-update'}
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
                  />
                </React.Fragment>
              ))}
            </RowPermissionsSectionWrapper>

            {queryType !== 'delete' && (
              <ColumnPermissionsSection
                roleName={roleName}
                queryType={queryType}
                columns={formData?.columns}
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

            <hr className="my-4" />

            {/* {!!tableNames?.length && (
            <ClonePermissionsSection
              queryType={queryType}
              tables={tableNames}
              supportedQueryTypes={supportedQueries}
              roles={allRoles}
            />
          )} */}

            <div className="pt-2 flex gap-2">
              <Button
                type="submit"
                mode="primary"
                title={
                  filterType === 'none'
                    ? 'You must select an option for row permissions'
                    : 'Submit'
                }
                disabled={filterType === 'none'}
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
        );
      }}
    </Form>
  );
};

export default PermissionsForm;
