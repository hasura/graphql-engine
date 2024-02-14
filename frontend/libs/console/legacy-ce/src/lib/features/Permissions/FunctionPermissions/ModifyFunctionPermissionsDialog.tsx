import { Dialog } from '../../../new-components/Dialog';
import {
  MetadataFunction,
  QualifiedFunction,
} from '../../hasura-metadata-types';
import { areTablesEqual, useMetadata } from '../../hasura-metadata-api';
import { FaCheck, FaTimes } from 'react-icons/fa';
import React from 'react';
import { Switch } from '../../../new-components/Switch';
import clsx from 'clsx';
import { hasuraToast } from '../../../new-components/Toasts';
import { QueryClient, useQueryClient } from 'react-query';
import { IndicatorCard } from '../../../new-components/IndicatorCard';
import { Link } from 'react-router';
import { LearnMoreLink } from '../../../new-components/LearnMoreLink';
import { getMetadataDataSource } from './utils';
import { useManageFunctionPermission } from '../../MetadataAPI/hooks/useManageFunctionPermission';
import { MetadataHelpers } from '../../hasura-metadata-api/metadataHelpers';

export type ModifyFunctionPermissionsProps = {
  qualifiedFunction: QualifiedFunction[];
  dataSourceName: string;
  onClose: () => void;
};

const SavingPermissionsLoader = () => (
  <svg
    aria-hidden="true"
    className="w-4 h-4 mr-2 text-gray-200 animate-spin dark:text-gray-600 fill-amber-600"
    viewBox="0 0 100 101"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M100 50.5908C100 78.2051 77.6142 100.591 50 100.591C22.3858 100.591 0 78.2051 0 50.5908C0 22.9766 22.3858 0.59082 50 0.59082C77.6142 0.59082 100 22.9766 100 50.5908ZM9.08144 50.5908C9.08144 73.1895 27.4013 91.5094 50 91.5094C72.5987 91.5094 90.9186 73.1895 90.9186 50.5908C90.9186 27.9921 72.5987 9.67226 50 9.67226C27.4013 9.67226 9.08144 27.9921 9.08144 50.5908Z"
      fill="currentColor"
    />
    <path
      d="M93.9676 39.0409C96.393 38.4038 97.8624 35.9116 97.0079 33.5539C95.2932 28.8227 92.871 24.3692 89.8167 20.348C85.8452 15.1192 80.8826 10.7238 75.2124 7.41289C69.5422 4.10194 63.2754 1.94025 56.7698 1.05124C51.7666 0.367541 46.6976 0.446843 41.7345 1.27873C39.2613 1.69328 37.813 4.19778 38.4501 6.62326C39.0873 9.04874 41.5694 10.4717 44.0505 10.1071C47.8511 9.54855 51.7191 9.52689 55.5402 10.0491C60.8642 10.7766 65.9928 12.5457 70.6331 15.2552C75.2735 17.9648 79.3347 21.5619 82.5849 25.841C84.9175 28.9121 86.7997 32.2913 88.1811 35.8758C89.083 38.2158 91.5421 39.6781 93.9676 39.0409Z"
      fill="currentFill"
    />
  </svg>
);

const togglePermission =
  ({
    role,
    qualifiedFunction,
    functionDefinition,
    queryClient,
    setIsSaving,
    createFunctionPermission,
    deleteFunctionPermission,
  }: {
    role: string;
    qualifiedFunction: QualifiedFunction;
    functionDefinition: MetadataFunction | undefined;
    queryClient: QueryClient;
    setIsSaving: (isSaving: boolean) => void;
    createFunctionPermission: ({
      qualifiedFunction,
      role,
      callbacks,
    }: {
      qualifiedFunction: unknown;
      role: string;
      callbacks?: Array<() => void>;
    }) => void;
    deleteFunctionPermission: ({
      qualifiedFunction,
      role,
      callbacks,
    }: {
      qualifiedFunction: unknown;
      role: string;
      callbacks?: Array<() => void>;
    }) => void;
  }) =>
  async (checked: boolean) => {
    const newFunctionDefinition = {
      ...functionDefinition,
    };
    const callbacks = {
      onSuccess: () => {
        setIsSaving(false);
        MetadataHelpers.invalidate();
        hasuraToast({
          type: 'success',
          title: 'Success!',
          message: 'Permissions saved successfully!',
        });
      },
      onError: (err: Error) => {
        setIsSaving(false);
        hasuraToast({
          type: 'error',
          title: 'Error!',
          message: 'Something went wrong while saving permissions',
          ...(err.message
            ? {
                children: <pre className="text-xs">{err.message}</pre>,
              }
            : {}),
        });
      },
    };

    setIsSaving(true);
    if (checked) {
      newFunctionDefinition.permissions = [
        ...(functionDefinition?.permissions
          ? functionDefinition.permissions
          : []),
        { role },
      ];
      createFunctionPermission({
        qualifiedFunction,
        role,
        ...callbacks,
      });
    } else {
      const newPermissions =
        functionDefinition?.permissions?.filter(
          permissions => permissions.role !== role
        ) || [];
      if (newPermissions?.length > 0) {
        newFunctionDefinition.permissions = newPermissions;
      }
      deleteFunctionPermission({
        qualifiedFunction,
        role,
        ...callbacks,
      });
    }
  };

export const ModifyFunctionPermissionsDialog = ({
  qualifiedFunction,
  dataSourceName,
  onClose,
}: ModifyFunctionPermissionsProps) => {
  const queryClient = useQueryClient();
  const [isSaving, setIsSaving] = React.useState(false);
  const { createFunctionPermission, deleteFunctionPermission } =
    useManageFunctionPermission({ dataSourceName });
  const { data, isLoading } = useMetadata(m =>
    getMetadataDataSource(dataSourceName)(m)
  );

  if (isLoading) {
    return (
      <div>
        <SavingPermissionsLoader />
        Loading...
      </div>
    );
  }

  const functionDefinition = data?.functions?.find(func =>
    areTablesEqual(func.function, qualifiedFunction)
  );
  const returnedTable = (
    functionDefinition?.configuration?.response?.table as string[]
  )[0];
  const permissions = data?.tables?.find(table =>
    areTablesEqual(table.table, [returnedTable])
  );

  return (
    <Dialog
      hasBackdrop
      size="lg"
      title="Edit Function Permissions"
      onClose={onClose}
      footer={<Dialog.Footer onSubmit={onClose} callToAction="Close" />}
    >
      <div className="m-4 relative">
        {isSaving ? (
          <div className="absolute inset-0 bg-gray-100 bg-opacity-80 px-md py-sm text-sm font-semibold text-muted flex items-center justify-center z-10">
            <SavingPermissionsLoader />
            Saving permissions...
          </div>
        ) : null}
        <div>
          <div>
            <IndicatorCard status="info">
              Permissions are <strong>inherited</strong> from the{' '}
              <strong>SELECT permissions</strong> of the referenced{' '}
              <strong>
                table&nbsp;
                {returnedTable}
              </strong>
              .
              <LearnMoreLink
                href={
                  'https://hasura.io/docs/latest/schema/postgres/custom-functions/#permissions-for-custom-functions'
                }
              />
              <br />
              The function will be <strong>exposed to the role</strong> if{' '}
              <strong>table SELECT permissions</strong> are enabled and{' '}
              <strong>function permissions</strong> are enabled for the role.
              <br />
              <Link
                to={`/data/v2/manage/table/permissions?database=${dataSourceName}&table=["${returnedTable}"]`}
              >
                Go to <strong>{returnedTable}</strong> table permissions.
              </Link>
            </IndicatorCard>
          </div>
          <table className="min-w-full divide-y divide-gray-200 text-left border border-gray-300 rounded">
            <thead>
              <tr className="divide-x divide-gray-300">
                <th className="w-0 min-w-[200px] bg-gray-50 border-r border-gray-300 px-md py-sm text-sm font-semibold text-muted uppercase tracking-wider">
                  Role
                </th>
                <th className="bg-gray-50 px-md py-sm text-sm font-semibold text-muted text-center uppercase tracking-wider">
                  Permissions
                </th>
              </tr>
            </thead>
            <tbody className="bg-white divide-y divide-gray-300">
              <tr key="admin" className="group divide-x divide-gray-300">
                <td className="w-0 bg-gray-50 px-md py-sm font-semibold text-muted">
                  Admin
                </td>
                <td className="px-md py-sm bg-gray-50 whitespace-nowrap text-center cursor-not-allowed">
                  <FaCheck className="text-green-600" size={20} />
                </td>
              </tr>
              {permissions?.select_permissions?.map(permission => {
                return (
                  <tr
                    key={
                      permission.role +
                      functionDefinition?.permissions?.includes({
                        role: permission.role,
                      })
                    }
                    className="group divide-x divide-gray-300"
                  >
                    <td className="w-0 bg-gray-50 px-md py-sm font-semibold text-muted">
                      {permission.role}
                    </td>
                    <td className="px-md py-sm whitespace-nowrap text-center bg-white">
                      <div className="flex items-center justify-center gap-2">
                        <FaTimes
                          size={
                            functionDefinition?.permissions?.find(
                              functionPermission =>
                                permission.role === functionPermission.role
                            ) !== undefined
                              ? 14
                              : 20
                          }
                          className={clsx(
                            'text-red-600',
                            functionDefinition?.permissions?.find(
                              functionPermission =>
                                permission.role === functionPermission.role
                            ) !== undefined
                              ? 'opacity-30 grayscale m-1'
                              : 'mr-px'
                          )}
                        />
                        <Switch
                          className={
                            functionDefinition?.permissions?.find(
                              functionPermission =>
                                permission.role === functionPermission.role
                            ) !== undefined
                              ? ''
                              : 'bg-red-600'
                          }
                          checked={
                            functionDefinition?.permissions?.find(
                              functionPermission =>
                                permission.role === functionPermission.role
                            ) !== undefined
                          }
                          onCheckedChange={togglePermission({
                            role: permission.role,
                            qualifiedFunction,
                            functionDefinition,
                            createFunctionPermission,
                            deleteFunctionPermission,
                            setIsSaving,
                            queryClient,
                          })}
                          data-testid={`toggle-permission-${permission.role}`}
                        />
                        <FaCheck
                          size={
                            functionDefinition?.permissions?.find(
                              functionPermission =>
                                permission.role === functionPermission.role
                            ) !== undefined
                              ? 20
                              : 14
                          }
                          className={clsx(
                            'text-green-600',
                            functionDefinition?.permissions?.find(
                              functionPermission =>
                                permission.role === functionPermission.role
                            ) !== undefined
                              ? 'ml-px'
                              : 'opacity-30 grayscale m-1'
                          )}
                        />
                      </div>
                    </td>
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
      </div>
    </Dialog>
  );
};
