import React from 'react';
import { useWatch, useFormContext } from 'react-hook-form';
import { z } from 'zod';
import { Form } from '@/new-components/Form';
import { FaInfo } from 'react-icons/fa';

import { QUERY_TYPES, Operations } from '@/dataSources';

import { arrayDiff } from '../../components/Common/utils/jsUtils';

import { useRolePermissions } from './hooks/usePermissions';
import { PermissionsLegend } from './components/PermissionsLegend';
import { EditableCell, InputCell } from './components/Cells';

type QueryType = 'insert' | 'select' | 'update' | 'delete';

interface ViewPermissionsNoteProps {
  viewsSupported: boolean;
  supportedQueryTypes: Operations[];
}

export const ViewPermissionsNote: React.FC<ViewPermissionsNoteProps> = ({
  viewsSupported,
  supportedQueryTypes,
}) => {
  if (!viewsSupported) {
    return null;
  }

  const unsupportedQueryTypes = arrayDiff(QUERY_TYPES, supportedQueryTypes);

  if (unsupportedQueryTypes.length) {
    return (
      <div className="">
        <FaInfo aria-hidden="true" />
        &nbsp; You cannot {unsupportedQueryTypes.join('/')} into this view
      </div>
    );
  }

  return null;
};

export interface PermissionsTableProps {
  schemaName: string;
  tableName: string;
  selected: Selection | null;
  onChange: (input: any) => void;
}

export interface OnChangeArgs {
  changeType: 'bulk' | 'open' | 'close';
  data?: Selection | string[];
}

export interface Selection {
  queryType: QueryType;
  roleName: string;
  accessType: string;
  isNewRole?: boolean;
}

export const PermissionsTable: React.FC<PermissionsTableProps> = ({
  schemaName,
  tableName,
  selected,
  onChange,
}) => {
  const { control, setFocus } = useFormContext<TableFormOutput>();

  const bulkSelected = useWatch({ control, name: 'bulkSelected' });
  const newRoleName = useWatch({ control, name: 'newRoleName' });

  const { supportedQueries, rolePermissions } = useRolePermissions({
    schema: schemaName,
    name: tableName,
  });

  const clickHandler = ({
    roleName,
    queryType,
    accessType,
    isNewRole,
  }: Selection) => {
    if (queryType === selected?.queryType && roleName === selected?.roleName) {
      return onChange({ changeType: 'close' });
    }

    if (isNewRole && !newRoleName) {
      onChange({ changeType: 'close' });
      return setFocus('newRoleName');
    }

    onChange({
      changeType: 'open',
      data: { roleName, queryType, accessType },
    });
  };

  React.useEffect(() => {
    onChange({ changeType: 'bulk', data: bulkSelected });
  }, [bulkSelected, onChange]);

  return (
    <>
      <PermissionsLegend />

      <div className="overflow-x-auto border border-gray-300 rounded">
        <table className="min-w-full divide-y divide-gray-200 text-left">
          <thead>
            <tr className="divide-x divide-gray-300">
              <th className="w-0 bg-gray-50 border-r border-gray-200 px-md py-sm text-sm font-semibold text-muted uppercase tracking-wider">
                ROLE
              </th>
              {supportedQueries.map(supportedQuery => (
                <th
                  className="bg-gray-50 px-md py-sm text-sm font-semibold text-muted text-center uppercase tracking-wider"
                  key={supportedQuery}
                >
                  {supportedQuery.toUpperCase()}
                </th>
              ))}
            </tr>
          </thead>

          <tbody className="bg-white divide-y divide-gray-300">
            {rolePermissions.map(
              ({ roleName, isNewRole, permissionTypes, bulkSelect }) => (
                <tr key={roleName} className="group divide-x divide-gray-300">
                  <InputCell
                    roleName={roleName}
                    isNewRole={isNewRole}
                    isSelectable={bulkSelect.isSelectable}
                  />

                  {permissionTypes.map(({ permissionType, access }) => {
                    const isEditable = roleName !== 'admin';

                    if (isNewRole) {
                      return (
                        <EditableCell
                          key={permissionType}
                          isEditable={isEditable}
                          access={access}
                          aria-label={`${newRoleName}-${permissionType}`}
                          isCurrentEdit={
                            permissionType === selected?.queryType &&
                            newRoleName === selected?.roleName
                          }
                          onClick={() =>
                            clickHandler({
                              roleName: newRoleName,
                              queryType: permissionType,
                              accessType: access,
                              isNewRole: true,
                            })
                          }
                        />
                      );
                    }

                    return (
                      <EditableCell
                        key={permissionType}
                        isEditable={isEditable}
                        access={access}
                        aria-label={`${roleName}-${permissionType}`}
                        isCurrentEdit={
                          permissionType === selected?.queryType &&
                          roleName === selected?.roleName
                        }
                        onClick={() =>
                          clickHandler({
                            roleName,
                            queryType: permissionType,
                            accessType: access,
                          })
                        }
                      />
                    );
                  })}
                </tr>
              )
            )}
          </tbody>
        </table>
      </div>
    </>
  );
};

const schema = z.object({
  selected: z.object({
    queryType: z.string(),
    roleName: z.string(),
  }),
  bulkSelected: z.optional(z.array(z.string())),
  newRoleName: z.string(),
});

export type TableFormOutput = z.infer<typeof schema>;

export const TableForm: React.FC = ({ children }) => (
  <Form
    schema={schema}
    options={{
      defaultValues: {
        bulkSelected: undefined,
        newRoleName: '',
      },
    }}
    onSubmit={() => {}}
  >
    {() => children}
  </Form>
);
