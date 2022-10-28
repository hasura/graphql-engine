import React from 'react';

import { Form } from '@/new-components/Form';
import { Button } from '@/new-components/Button';

import { schema } from './utils/formSchema';

import { AccessType, FormOutput, QueryType } from './types';
import {
  AggregationSection,
  BackendOnlySection,
  ColumnPermissionsSection,
  ColumnPresetsSection,
  RowPermissionsSection,
  RowPermissionsSectionWrapper,
} from './components';

import { useFormData, useDefaultValues, useUpdatePermissions } from './hooks';

export interface PermissionsFormProps {
  currentSource: string;
  dataSourceName: string;
  table: unknown;
  queryType: QueryType;
  roleName: string;
  accessType: AccessType;
  handleClose: () => void;
}

export const PermissionsForm = (props: PermissionsFormProps) => {
  const {
    currentSource,
    dataSourceName,
    table,
    queryType,
    roleName,
    accessType,
    handleClose,
  } = props;

  // loads all information about selected table
  // e.g. column names, supported queries etc.
  const {
    data,
    isLoading: loadingFormData,
    isError: formDataError,
  } = useFormData({
    dataSourceName,
    table,
    queryType,
    roleName,
  });

  // loads any existing permissions from the metadata
  const {
    data: defaultValues,
    isLoading: defaultValuesLoading,
    isError: defaultValuesError,
  } = useDefaultValues({
    dataSourceName,
    table,
    roleName,
    queryType,
  });

  // functions fired when the form is submitted
  const { updatePermissions, deletePermissions } = useUpdatePermissions({
    currentSource,
    dataSourceName,
    table,
    queryType,
    roleName,
    accessType,
  });

  const handleSubmit = async (formData: Record<string, unknown>) => {
    await updatePermissions.submit(formData as FormOutput);
    handleClose();
  };

  const handleDelete = async () => {
    await deletePermissions.submit([queryType]);
    handleClose();
  };

  const isError = formDataError || defaultValuesError;

  const isSubmittingError =
    updatePermissions.isError || deletePermissions.isError;

  const isLoading = loadingFormData || defaultValuesLoading;

  // allRowChecks relates to other queries and is for duplicating from others
  const allRowChecks = defaultValues?.allRowChecks;

  // for update it is possible to set pre update and post update row checks
  const rowPermissions = queryType === 'update' ? ['pre', 'post'] : [queryType];

  if (isSubmittingError) {
    return <div>Error submitting form</div>;
  }

  // these will be replaced by components once spec is decided
  if (isError) {
    return <div>Error loading form data</div>;
  }

  // these will be replaced by components once spec is decided
  if (isLoading) {
    return <div>Loading...</div>;
  }

  return (
    <Form
      key={`${queryType}-${roleName}-${accessType}`}
      onSubmit={handleSubmit}
      schema={schema}
      options={{ defaultValues }}
    >
      {options => {
        console.log('form values---->', options.getValues());
        console.log('form errors---->', options.formState.errors);
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
                  />
                </React.Fragment>
              ))}
            </RowPermissionsSectionWrapper>

            {queryType !== 'delete' && (
              <ColumnPermissionsSection
                roleName={roleName}
                queryType={queryType}
                columns={data?.columns}
              />
            )}

            {['insert', 'update'].includes(queryType) && (
              <ColumnPresetsSection
                queryType={queryType}
                columns={data?.columns}
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
