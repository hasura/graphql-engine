import { MetadataTable } from '../../../hasura-metadata-types/source/table';

const formatColumns = (columns: string[] | Record<string, boolean>) => {
  if (!Array.isArray(columns)) return columns || {};
  return (
    columns?.reduce((tally, column) => {
      return { ...tally, [column]: true };
    }, {}) || {}
  );
};

const getPermissionsMappedByRole = ({
  tableData,
  key,
  currentRole,
  currentQueryType,
  tally,
  queryType,
}: {
  tableData: MetadataTable;
  key:
    | 'delete_permissions'
    | 'insert_permissions'
    | 'select_permissions'
    | 'update_permissions';
  currentRole: string;
  currentQueryType: string;
  tally: Record<string, any>[];
  queryType: string;
}) => {
  const permissions =
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    tableData?.[key]?.find(
      (data: { role: string }) => data.role === currentRole
    )?.permission;
  if (!permissions) return tally;

  if (currentQueryType === 'update' || currentQueryType === 'insert') {
    permissions.check = permissions?.filter;
  } else if (currentQueryType === 'select' || currentQueryType === 'delete') {
    permissions.filter = permissions?.check;
  }
  permissions.columns = formatColumns(permissions.columns);

  return [
    ...tally,
    {
      queryType,
      data: permissions,
    },
  ];
};

export const getNonSelectedQueryTypePermissions = (
  tableData: MetadataTable,
  currentQueryType: string,
  currentRole: string
) => {
  if (!tableData) return [];
  const metadataKeys = Object.keys(tableData);

  const existingPermissions = metadataKeys
    ?.filter(
      key =>
        key === 'update_permissions' ||
        key === 'select_permissions' ||
        key === 'delete_permissions' ||
        key === 'insert_permissions'
    )
    ?.reduce((tally: Record<string, any>[], key: string) => {
      if (key === 'select_permissions' && currentQueryType !== 'select') {
        return getPermissionsMappedByRole({
          tableData,
          key,
          currentRole,
          currentQueryType,
          tally,
          queryType: 'select',
        });
      }
      if (key === 'update_permissions' && currentQueryType !== 'update') {
        return getPermissionsMappedByRole({
          tableData,
          key,
          currentRole,
          currentQueryType,
          tally,
          queryType: 'update',
        });
      }

      if (key === 'delete_permissions' && currentQueryType !== 'delete') {
        return getPermissionsMappedByRole({
          tableData,
          key,
          currentRole,
          currentQueryType,
          tally,
          queryType: 'delete',
        });
      }

      if (key === 'insert_permissions' && currentQueryType !== 'insert') {
        return getPermissionsMappedByRole({
          tableData,
          key,
          currentRole,
          currentQueryType,
          tally,
          queryType: 'insert',
        });
      }
      return tally;
    }, []);

  return existingPermissions;
};
