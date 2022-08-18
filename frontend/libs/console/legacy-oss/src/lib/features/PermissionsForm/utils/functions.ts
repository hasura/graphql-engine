import { Permission } from '@/dataSources/types';

interface Args {
  permissions?: Permission[];
  roleName: string;
}

export const getCurrentRole = ({ permissions, roleName }: Args) => {
  const rolePermissions = permissions?.find(
    ({ role_name }) => role_name === roleName
  );

  return rolePermissions;
};
