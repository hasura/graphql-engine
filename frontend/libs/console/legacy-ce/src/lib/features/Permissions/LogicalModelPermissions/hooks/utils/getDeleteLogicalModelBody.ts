import { mapPostgresToPg } from '.';
import { Source } from '../../../../hasura-metadata-types';
import { Permission } from '../../components/types';
export interface DeleteLogicalModalBodyArgs {
  logicalModelName: string;
  permission: Permission;
  source: Source;
}

type PermissionArgsType = {
  name: string;
  role: string;
  source: string;
};

type PermissionBodyType = {
  type: string;
  args: PermissionArgsType;
};

export const getDeleteLogicalModelBody = ({
  logicalModelName,
  permission,
  source,
}: DeleteLogicalModalBodyArgs): PermissionBodyType[] => {
  const args = [
    {
      type: `${mapPostgresToPg(source.kind)}_drop_logical_model_${
        permission.action
      }_permission`,
      args: {
        name: logicalModelName,
        role: permission.roleName,
        source: source.name,
      },
    },
  ];

  return args;
};
