import {
  RowPermissionsSectionType,
  getRowPermission,
} from '../components/RowPermissions';

export const copyQueryTypePermissions = (
  permissoinType: RowPermissionsSectionType,
  queryType: RowPermissionsSectionType,
  subQueryType: 'pre_update' | 'post_update' | undefined,
  data: Record<string, any>
): [string, Record<string, string>] => {
  const mappedType = getRowPermission(permissoinType);
  if (subQueryType) {
    // This only applies to the visual section of 'pre_update' and 'post_update'
    const mappedsubQueryType = getRowPermission(subQueryType);
    return [mappedsubQueryType, data[mappedType]];
  }
  // This only applies to the real query types 'select', 'insert', 'update', 'delete'
  // It has to take the special cases of 'pre_update' and 'post_update' into account when mapping values from the update permissions
  const mappedQueryType = getRowPermission(queryType);
  return [mappedQueryType, data[mappedType]];
};
