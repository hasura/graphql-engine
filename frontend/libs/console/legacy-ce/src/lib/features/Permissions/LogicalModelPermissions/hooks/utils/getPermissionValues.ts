type GetPermissionValuesInputType = {
  roleName: string;
  filter?: Record<string, unknown>;
  check?: Record<string, unknown>;
  columns?: string[];
  action: string;
  isNew: boolean;
  source: string;
};

type GetPermissionValuesOutputType = {
  filter?: Record<string, unknown>;
  check?: Record<string, unknown>;
  columns?: string[];
};

export const getPermissionValues = (
  obj: GetPermissionValuesInputType
): GetPermissionValuesOutputType => {
  const { filter, check, columns } = obj;
  const result: GetPermissionValuesOutputType = {};

  if (filter !== undefined) {
    result.filter = filter;
  }

  if (check !== undefined) {
    result.check = check;
  }

  if (columns !== undefined) {
    result.columns = columns;
  }

  return result;
};
