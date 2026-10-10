import React from 'react';
import { FaInfo } from 'react-icons/fa';
import { IndicatorCard, SkeletonList } from '@hasura/shared/ui';
import { useRolePermissions } from './hooks/usePermissions';
import { PermissionsLegend } from './components/PermissionsLegend';
import {
  PermissionsTableView,
  PermissionsTableRow,
} from './components/PermissionsTableView';
import { TableMachine } from './hooks';
import { Capabilities } from '@hasura/dc-api-types';
import { getDriversSupportedQueryTypes } from './utils/getDriversSupportedQueryTypes';
import { isPermissionCheckboxDisabled } from './utils/isPermissionCheckboxDisabled';
import {
  DATA_QUERY_TYPES,
  DataQueryType,
  Source,
  Table as TableInfo,
} from '@hasura/shared/types';
import {
  useDriverCapabilities,
  useIsTableView,
} from '@hasura/metadata/data-source';

const getIsColumnEditable = (
  roleName: string,
  isView: boolean | undefined,
  driverSupportedQueries: string[],
  permissionType: string,
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
  supportedQueryTypes: DataQueryType[];
}

export const ViewPermissionsNote: React.FC<ViewPermissionsNoteProps> = ({
  viewsSupported,
  supportedQueryTypes,
}) => {
  if (!viewsSupported) {
    return null;
  }

  const unsupportedQueryTypes = DATA_QUERY_TYPES.filter(
    (query) => !supportedQueryTypes.includes(query),
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
  source: Source;
  table: TableInfo;
  machine: ReturnType<TableMachine>;
}

export interface Selection {
  queryType: DataQueryType;
  roleName: string;
  accessType: string;
  isNewRole?: boolean;
}

export const PermissionsTable: React.FC<PermissionsTableProps> = ({
  source,
  table,
  machine,
}) => {
  const { data, isLoading } = useRolePermissions({
    dataSourceName: source.name,
    table,
  });

  const driverCapabilities = useDriverCapabilities({ source });
  const driverSupportedQueries = getDriversSupportedQueryTypes(
    driverCapabilities?.data as Capabilities,
  );

  const [state, send] = machine;

  const { data: isView } = useIsTableView({ source, table });

  if (isLoading)
    return (
      <div>
        <SkeletonList count={5} containerClassName="my-1.5" />
      </div>
    );

  if (!data) {
    return (
      <div>
        <IndicatorCard status="negative" headline="Error" showIcon>
          Something went wrong while fetching permissions
        </IndicatorCard>
      </div>
    );
  }

  const { supportedQueries, rolePermissions } = data;

  const columns = supportedQueries.map((supportedQuery) => ({
    key: supportedQuery,
    label: supportedQuery.toUpperCase(),
  }));

  const rows: PermissionsTableRow[] = rolePermissions.map(
    ({ roleName, isNewRole, permissionTypes, bulkSelect }) => ({
      roleName,
      roleCell: isNewRole
        ? {
            isNewRole: true,
            newRoleValue: state.context.newRoleName,
            onNewRoleValueChange: (newRoleName) =>
              send({ type: 'NEW_ROLE_NAME', newRoleName }),
          }
        : {
            isSelectable: bulkSelect.isSelectable,
            isSelected: state.context.bulkSelections.includes(roleName),
            onSelectChange: () => send({ type: 'BULK_OPEN', roleName }),
            disabled: isPermissionCheckboxDisabled(permissionTypes),
          },
      cells: Object.fromEntries(
        permissionTypes.map(({ permissionType, access }) => {
          const isEditable = getIsColumnEditable(
            roleName,
            isView,
            driverSupportedQueries,
            permissionType,
          );

          if (isNewRole) {
            return [
              permissionType,
              {
                isEditable,
                access,
                'aria-label': `${state.context.newRoleName}-${permissionType}`,
                testId: `permission-table-button-${roleName}-${permissionType}`,
                isCurrentEdit:
                  permissionType === state.context.selectedForm.queryType &&
                  state.context.newRoleName ===
                    state.context.selectedForm.roleName,
                onClick: () => {
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
                },
              },
            ];
          }

          return [
            permissionType,
            {
              isEditable,
              access,
              'aria-label': `${roleName}-${permissionType}`,
              testId: `permission-table-button-${roleName}-${permissionType}`,
              isCurrentEdit:
                permissionType === state.context.selectedForm.queryType &&
                roleName === state.context.selectedForm.roleName,
              onClick: () =>
                send({
                  type: 'FORM_OPEN',
                  selectedForm: {
                    roleName,
                    queryType: permissionType,
                    accessType: access,
                  },
                }),
            },
          ];
        }),
      ),
    }),
  );

  return (
    <>
      <PermissionsLegend />

      <PermissionsTableView columns={columns} rows={rows} />
    </>
  );
};
