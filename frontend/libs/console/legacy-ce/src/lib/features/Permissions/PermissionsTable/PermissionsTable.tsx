import React from 'react';
import { FaInfo } from 'react-icons/fa';
import Skeleton from 'react-loading-skeleton';
import { IndicatorCard } from '../../../new-components/IndicatorCard';
import { useRolePermissions } from './hooks/usePermissions';
import { PermissionsLegend } from './components/PermissionsLegend';
import { EditableCell, InputCell } from './components/Cells';
import { TableMachine } from './hooks';
import { useDriverCapabilities } from '../../Data/hooks/useDriverCapabilities';
import { Capabilities } from '@hasura/dc-api-types';
import { getDriversSupportedQueryTypes } from './utils/getDriversSupportedQueryTypes';
import { useIsTableView } from '../../Data/hooks/useIsTableView';
import { isPermissionCheckboxDisabled } from './utils/isPermissionCheckboxDisabled';

const queryType = ['insert', 'select', 'update', 'delete'] as const;
type QueryType = (typeof queryType)[number];

const getIsColumnEditable = (
  roleName: string,
  isView: boolean | undefined,
  driverSupportedQueries: string[],
  permissionType: string
) => {
  if (roleName === 'admin') return false;

  if (isView) {
    return permissionType === 'select';
  }
  if (driverSupportedQueries.includes(permissionType)) return true;

  return false;
};

interface ViewPermissionsNoteProps {
  viewsSupported: boolean;
  supportedQueryTypes: QueryType[];
}

export const ViewPermissionsNote: React.FC<ViewPermissionsNoteProps> = ({
  viewsSupported,
  supportedQueryTypes,
}) => {
  if (!viewsSupported) {
    return null;
  }

  const unsupportedQueryTypes = queryType.filter(
    query => !supportedQueryTypes.includes(query)
  );

  if (unsupportedQueryTypes.length) {
    return (
      <div className="">
        <FaInfo aria-hidden="true" />
        &nbsp; You cannot {unsupportedQueryTypes.join('/')} into this view
      </div>
    );
  }

  return null;
};

export interface PermissionsTableProps {
  dataSourceName: string;
  table: unknown;
  machine: ReturnType<TableMachine>;
}

export interface Selection {
  queryType: QueryType;
  roleName: string;
  accessType: string;
  isNewRole?: boolean;
}

export const PermissionsTable: React.FC<PermissionsTableProps> = ({
  dataSourceName,
  table,
  machine,
}) => {
  const { data, isLoading } = useRolePermissions({
    dataSourceName,
    table,
  });

  const driverCapabilities = useDriverCapabilities({ dataSourceName });
  const driverSupportedQueries = getDriversSupportedQueryTypes(
    driverCapabilities?.data as Capabilities
  );

  const [state, send] = machine;

  const { data: isView } = useIsTableView({ dataSourceName, table });

  if (isLoading)
    return (
      <div>
        <Skeleton count={5} height={30} className="my-1.5" />
      </div>
    );

  if (!data) {
    return (
      <div>
        <IndicatorCard status="negative" headline="Error">
          Something went wrong while fetching permissions
        </IndicatorCard>
      </div>
    );
  }

  const { supportedQueries, rolePermissions } = data;

  return (
    <>
      <PermissionsLegend />

      <div className="overflow-x-auto border border-gray-300 rounded">
        <table className="min-w-full divide-y divide-gray-200 text-left">
          <thead>
            <tr className="divide-x divide-gray-300">
              <th className="w-0 bg-gray-50 border-r border-gray-200 px-md py-sm text-sm font-semibold text-muted uppercase tracking-wider">
                ROLE
              </th>
              {supportedQueries.map(supportedQuery => (
                <th
                  className="bg-gray-50 px-md py-sm text-sm font-semibold text-muted text-center uppercase tracking-wider"
                  key={supportedQuery}
                >
                  {supportedQuery.toUpperCase()}
                </th>
              ))}
            </tr>
          </thead>

          <tbody className="bg-white divide-y divide-gray-300">
            {rolePermissions.map(
              ({ roleName, isNewRole, permissionTypes, bulkSelect }) => (
                <tr key={roleName} className="group divide-x divide-gray-300">
                  <InputCell
                    roleName={roleName}
                    isNewRole={isNewRole}
                    isSelectable={bulkSelect.isSelectable}
                    isSelected={state.context.bulkSelections.includes(roleName)}
                    machine={machine}
                    disabled={isPermissionCheckboxDisabled(permissionTypes)}
                  />

                  {permissionTypes.map(({ permissionType, access }) => {
                    // TODO: add checks to see what permissions are supported by each db

                    const isEditable = getIsColumnEditable(
                      roleName,
                      isView,
                      driverSupportedQueries,
                      permissionType
                    );

                    if (isNewRole) {
                      return (
                        <EditableCell
                          key={permissionType}
                          isEditable={isEditable}
                          access={access}
                          aria-label={`${state.context.newRoleName}-${permissionType}`}
                          testId={`permission-table-button-${roleName}-${permissionType}`}
                          isCurrentEdit={
                            permissionType ===
                              state.context.selectedForm.queryType &&
                            state.context.newRoleName ===
                              state.context.selectedForm.roleName
                          }
                          onClick={() => {
                            if (state.context.newRoleName !== '') {
                              send({
                                type: 'FORM_OPEN',
                                selectedForm: {
                                  roleName: state.context.newRoleName,
                                  queryType: permissionType,
                                  accessType: access,
                                  isNewRole: true,
                                },
                              });
                            } else {
                              send({
                                type: 'NEW_ROLE_NAME',
                                newRoleName: '',
                              });
                            }
                          }}
                        />
                      );
                    }

                    return (
                      <EditableCell
                        key={permissionType}
                        isEditable={isEditable}
                        access={access}
                        aria-label={`${roleName}-${permissionType}`}
                        testId={`permission-table-button-${roleName}-${permissionType}`}
                        isCurrentEdit={
                          permissionType ===
                            state.context.selectedForm.queryType &&
                          roleName === state.context.selectedForm.roleName
                        }
                        onClick={() =>
                          send({
                            type: 'FORM_OPEN',
                            selectedForm: {
                              roleName,
                              queryType: permissionType,
                              accessType: access,
                            },
                          })
                        }
                      />
                    );
                  })}
                </tr>
              )
            )}
          </tbody>
        </table>
      </div>
    </>
  );
};
