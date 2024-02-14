import { ReactNode } from 'react';
import { LogicalModelWithPermissions } from './types';
import { useLogicalModelPermissionsForm } from '../hooks/usePermissionForm';
import { FormProvider } from 'react-hook-form';

export function LogicalModelPermissionsFormProvider({
  children,
  logicalModel,
  roles,
}: {
  children: ReactNode;
  logicalModel: LogicalModelWithPermissions | undefined;
  roles: string[];
}) {
  const methods = useLogicalModelPermissionsForm(logicalModel, roles);
  return <FormProvider {...methods}>{children}</FormProvider>;
}
