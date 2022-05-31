import React from 'react';
import { useFormContext } from 'react-hook-form';

import { Form } from '@/new-components/Form';
import { Button } from '@/new-components/Button';

import {
  RowPermissionsSectionWrapper,
  RowPermissionsSection,
  ColumnPermissionsSection,
  ColumnPresetsSection,
  AggregationSection,
  BackendOnlySection,
  ClonePermissionsSection,
} from './components';

import { useDefaultValues, useFormData, useUpdatePermissions } from './hooks';
import { schema } from './utils/formSchema';

import { AccessType, FormOutput, QueryType } from './types';

export interface PermissionsFormProps {
  schemaName: string;
  tableName: string;
  queryType: QueryType;
  roleName: string;
  accessType: AccessType;
  handleClose: () => void;
}

interface ResetterProps {
  defaultValues: FormOutput;
}

// required to update the default values when the form switches between query types
// for example from update to select
const Resetter: React.FC<ResetterProps> = ({ defaultValues }) => {
  const { reset } = useFormContext();

  const initialRender = React.useRef(true);

  React.useEffect(() => {
    if (initialRender.current) {
      initialRender.current = false;
    } else {
      reset(defaultValues);
    }
  }, [defaultValues, reset]);

  return null;
};

export const PermissionsForm: React.FC<PermissionsFormProps> = ({
  schemaName,
  tableName,
  queryType,
  roleName,
  accessType,
  handleClose,
}) => {
  const {
    data,
    isLoading: loadingFormData,
    isError: formDataError,
  } = useFormData({
    schemaName,
    tableName,
    queryType,
    roleName,
  });

  const {
    data: defaults,
    isLoading: defaultValuesLoading,
    isError: defaultValuesError,
  } = useDefaultValues({
    schemaName,
    tableName,
    roleName,
    queryType,
  });

  const { updatePermissions, deletePermissions } = useUpdatePermissions({
    schemaName,
    tableName,
    queryType,
    roleName,
    accessType,
  });

  const handleSubmit = async (formData: FormOutput) => {
    updatePermissions.submit(formData);
    handleClose();
  };

  const handleDelete = async () => {
    await deletePermissions.submit([queryType]);
    handleClose();
  };

  const isError = formDataError || defaultValuesError;

  // these will be replaced by components once spec is decided
  if (isError) {
    return <div>Error loading form data</div>;
  }

  const isSubmittingError =
    updatePermissions.isError || deletePermissions.isError;

  // these will be replaced by components once spec is decided
  if (isSubmittingError) {
    return <div>Error submitting form data</div>;
  }

  const isLoading = loadingFormData || defaultValuesLoading;

  // these will be replaced by components once spec is decided
  if (isLoading) {
    return <div>Loading...</div>;
  }

  const {
    // i.e. tables that are not the currently open table (this is for cloning permissions)
    otherTableNames,
    allFunctions,
    roles,
    tables,
    supportedQueries,
    columns,
  } = data;

  // allRowChecks relates to other queries and is for duplicating from others
  // therefore it shouldn't be passed to the form as a default value
  const { allRowChecks, ...defaultValues } = defaults;

  // for update it is possible to set pre update and post update row checks
  const rowPermissions = queryType === 'update' ? ['pre', 'post'] : [queryType];

  return (
    <Form onSubmit={handleSubmit} schema={schema} options={{ defaultValues }}>
      {() => (
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
          <Resetter defaultValues={defaultValues} />

          <RowPermissionsSectionWrapper
            roleName={roleName}
            queryType={queryType}
            defaultOpen
          >
            {rowPermissions.map(permissionName => (
              <React.Fragment key={permissionName}>
                {/* if queryType is update 2 row permissions sections are rendered (pre and post) */}
                {/* therefore they need titles */}
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
                  queryType={queryType}
                  subQueryType={
                    queryType === 'update' ? permissionName : undefined
                  }
                  schemaName={schemaName}
                  tableName={tableName}
                  allRowChecks={allRowChecks}
                  allSchemas={tables}
                  allFunctions={allFunctions}
                />
              </React.Fragment>
            ))}
          </RowPermissionsSectionWrapper>

          {queryType !== 'delete' && (
            <ColumnPermissionsSection
              roleName={roleName}
              queryType={queryType}
              columns={columns}
            />
          )}

          {['insert', 'update'].includes(queryType) && (
            <ColumnPresetsSection queryType={queryType} columns={columns} />
          )}

          {queryType === 'select' && (
            <AggregationSection queryType={queryType} roleName={roleName} />
          )}

          {['insert', 'update', 'delete'].includes(queryType) && (
            <BackendOnlySection queryType={queryType} />
          )}

          <hr className="my-4" />

          {otherTableNames && roles && (
            <ClonePermissionsSection
              queryType={queryType}
              tables={otherTableNames}
              supportedQueryTypes={supportedQueries}
              roles={roles}
            />
          )}

          <hr className="my-4" />
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
      )}
    </Form>
  );
};

export default PermissionsForm;
