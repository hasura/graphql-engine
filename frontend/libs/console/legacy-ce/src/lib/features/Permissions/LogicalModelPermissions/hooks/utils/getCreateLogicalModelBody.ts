import { getPermissionValues } from './getPermissionValues';
import {
  LogicalModel,
  SingleMetadataTypes,
  Source,
} from '@hasura/shared/types';
import { Permission } from '../../components/types';
import { getDriverPrefix } from '@hasura/metadata/helpers';

export interface CreateLogicalModalBodyArgs {
  logicalModels: LogicalModel[];
  logicalModelName: string;
  permission: Permission;
  source: Source;
}

type PermissionArgsType = {
  name: string;
  role: string;
  permission?: Record<string, unknown>;
  source: string;
};

type PermissionBodyType = {
  type: SingleMetadataTypes;
  args: PermissionArgsType;
};

const doesRoleExist = (logicalModel: LogicalModel, roleName: string) => {
  const permissionKeys = ['select_permissions'] as ['select_permissions'];
  return permissionKeys.some(
    (key) =>
      Array.isArray(logicalModel[key]) &&
      logicalModel[key]?.some((permission) => permission.role === roleName),
  );
};

export const getCreateLogicalModelBody = ({
  logicalModelName,
  permission,
  logicalModels,
  source,
}: CreateLogicalModalBodyArgs): PermissionBodyType[] => {
  const permissionValues = getPermissionValues(permission);
  const args: PermissionBodyType[] = [
    {
      type: `${getDriverPrefix(source.kind)}_create_logical_model_${
        permission.action
      }_permission` as const,
      args: {
        name: logicalModelName,
        role: permission.roleName,
        permission: permissionValues,
        source: source.name,
      },
    },
  ];

  const permissionAlreadyExists = logicalModels.find((model: LogicalModel) =>
    doesRoleExist(model, permission.roleName),
  );

  if (permissionAlreadyExists) {
    args.unshift({
      type: `${getDriverPrefix(source.kind)}_drop_logical_model_${
        permission.action
      }_permission` as const,
      args: {
        permission: {},
        name: logicalModelName,
        role: permission.roleName,
        source: source.name,
      },
    });
  }

  return args;
};
