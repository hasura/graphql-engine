import { getPermissionValues } from './getPermissionValues';
import { LogicalModel, Source } from '../../../../hasura-metadata-types';
import { Permission } from '../../components/types';
import { mapPostgresToPg } from '.';

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
  type: string;
  args: PermissionArgsType;
};

const doesRoleExist = (logicalModel: LogicalModel, roleName: string) => {
  const permissionKeys = ['select_permissions'] as ['select_permissions'];
  return permissionKeys.some(
    key =>
      Array.isArray(logicalModel[key]) &&
      logicalModel[key]?.some(permission => permission.role === roleName)
  );
};

export const getCreateLogicalModelBody = ({
  logicalModelName,
  permission,
  logicalModels,
  source,
}: CreateLogicalModalBodyArgs): PermissionBodyType[] => {
  const permissionValues = getPermissionValues(permission);
  const args = [
    {
      type: `${mapPostgresToPg(source.kind)}_create_logical_model_${
        permission.action
      }_permission`,
      args: {
        name: logicalModelName,
        role: permission.roleName,
        permission: permissionValues,
        source: source.name,
      },
    },
  ];

  const permissionAlreadyExists = logicalModels.find((model: LogicalModel) =>
    doesRoleExist(model, permission.roleName)
  );

  if (permissionAlreadyExists) {
    args.unshift({
      type: `${mapPostgresToPg(source.kind)}_drop_logical_model_${
        permission.action
      }_permission`,
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
