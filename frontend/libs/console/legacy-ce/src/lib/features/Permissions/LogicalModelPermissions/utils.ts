import isEmpty from 'lodash/isEmpty';
import { AccessType, Permission } from './components/types';

export function permissionRowAccess(permission: Permission): AccessType {
  if (!permission.filter || isEmpty(permission.filter)) {
    return 'noAccess';
  } else {
    return 'fullAccess';
  }
}

export function permissionColumnAccess(
  permission: Permission,
  selectedColumns: string[]
): AccessType {
  if (permission.columns.length === 0) {
    return 'noAccess';
  } else if (permission.columns.length > selectedColumns.length) {
    return 'partialAccess';
  } else {
    return 'fullAccess';
  }
}
