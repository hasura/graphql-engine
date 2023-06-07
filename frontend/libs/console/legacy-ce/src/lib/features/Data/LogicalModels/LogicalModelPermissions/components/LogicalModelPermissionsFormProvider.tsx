import { ReactNode } from 'react';
import { LogicalModelWithPermissions } from './types';
import { useLogicalModelPermissionsForm } from '../hooks/usePermissionForm';
import { FormProvider } from 'react-hook-form';

export function LogicalModelPermissionsFormProvider({
  children,
  logicalModel,
}: {
  children: ReactNode;
  logicalModel: LogicalModelWithPermissions | undefined;
}) {
  const methods = useLogicalModelPermissionsForm(logicalModel);
  return <FormProvider {...methods}>{children}</FormProvider>;
}
