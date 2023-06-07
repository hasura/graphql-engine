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
  queryType: string;
}) => {
  const permissions =
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    tableData?.[key]?.find(
      (data: { role: string }) => data.role === currentRole
    )?.permission;
  if (!permissions) return null;
  if (currentQueryType === 'pre_update') {
    if (!permissions.check) permissions.check = permissions?.filter;
  }
  if (currentQueryType === 'post_update') {
    if (!permissions.filter) permissions.filter = permissions?.check;
  }
  if (currentQueryType === 'insert') {
    if (!permissions.check) permissions.check = permissions?.filter;
  } else if (currentQueryType === 'select' || currentQueryType === 'delete') {
    if (!permissions.filter) permissions.filter = permissions?.check;
  }
  permissions.columns = formatColumns(permissions.columns);

  return {
    queryType,
    data: permissions,
  };
};

export const getNonSelectedQueryTypePermissions = (
  tableData: MetadataTable | undefined,
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
    ?.reduce((tally: any[], key: string) => {
      if (key === 'select_permissions' && currentQueryType !== 'select') {
        return [
          ...tally,
          getPermissionsMappedByRole({
            tableData,
            key,
            currentRole,
            currentQueryType,
            queryType: 'select',
          }),
        ].filter(Boolean);
      }
      if (key === 'update_permissions' && currentQueryType !== 'update') {
        const pre = getPermissionsMappedByRole({
          tableData,
          key,
          currentRole,
          currentQueryType: 'pre_update',
          queryType: 'pre_update',
        });
        const post = getPermissionsMappedByRole({
          tableData,
          key,
          currentRole,
          currentQueryType: 'post_update',
          queryType: 'post_update',
        });

        return [...tally, pre, post].filter(Boolean);
      }

      if (key === 'delete_permissions' && currentQueryType !== 'delete') {
        return [
          ...tally,
          getPermissionsMappedByRole({
            tableData,
            key,
            currentRole,
            currentQueryType,
            queryType: 'delete',
          }),
        ].filter(Boolean);
      }

      if (key === 'insert_permissions' && currentQueryType !== 'insert') {
        return [
          ...tally,
          getPermissionsMappedByRole({
            tableData,
            key,
            currentRole,
            currentQueryType,
            queryType: 'insert',
          }),
        ].filter(Boolean);
      }

      return tally;
    }, []);

  return existingPermissions;
};
