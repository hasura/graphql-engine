import { getDriverPrefix } from '@hasura/metadata/helpers';
import { Source } from '@hasura/shared/types';
import { Permission } from '../../components/types';
import { TMigrationSingleQuery } from '@hasura/metadata/api';

export interface DeleteLogicalModalBodyArgs {
  logicalModelName: string;
  permission: Permission;
  source: Source;
}

export const getDeleteLogicalModelBody = ({
  logicalModelName,
  permission,
  source,
}: DeleteLogicalModalBodyArgs): TMigrationSingleQuery[] => {
  const args = [
    {
      type: `${getDriverPrefix(source.kind)}_drop_logical_model_${
        permission.action
      }_permission` as const,
      args: {
        name: logicalModelName,
        role: permission.roleName,
        source: source.name,
      },
    },
  ];

  return args;
};
