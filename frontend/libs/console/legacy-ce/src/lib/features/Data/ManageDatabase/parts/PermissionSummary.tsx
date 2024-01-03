import Helmet from 'react-helmet';
import { useURLParameters } from '../../ManageFunction/hooks/useUrlParameters';
import { useMetadata } from '../../../hasura-metadata-api';
import Select from 'react-select';
import { useEffect, useState } from 'react';
import { MetadataTable } from '../../../hasura-metadata-types';
import { FaCheck, FaEdit, FaTimes } from 'react-icons/fa';
import { useDispatch } from 'react-redux';
import { getRoute } from '../../../../utils/getDataRoute';
import _push from '../../../../components/Services/Data/push';
import { BreadCrumbs } from './BreadCrumbs';

type PermissionType =
  | 'select_permissions'
  | 'insert_permissions'
  | 'update_permissions'
  | 'delete_permissions';

const permissionType = [
  {
    label: 'Select permissions',
    value: 'select_permissions',
  },
  {
    label: 'Insert permissions',
    value: 'insert_permissions',
  },
  {
    label: 'Update permissions',
    value: 'update_permissions',
  },
  {
    label: 'Delete permissions',
    value: 'delete_permissions',
  },
];

export const PermissionSummary = () => {
  const [selectedPermissionType, setSelectedPermissionType] =
    useState<PermissionType>();
  const { data: metadata, refetch } = useMetadata();
  useEffect(() => {
    refetch();
  }, []);
  const dispatch = useDispatch();
  const urlData = useURLParameters(window.location);
  if (urlData.querystringParseResult === 'error')
    return <>Something went wrong while parsing the URL parameters</>;
  const { database } = urlData.data;
  const metadataObjectOfCurrentDatasource = metadata?.metadata.sources.filter(
    i => i.name === database
  );

  const dataSourceTables = metadataObjectOfCurrentDatasource?.map(
    i => i.tables
  );

  const getTableNames = (): string[] => {
    const tableNames: string[] = [];
    dataSourceTables?.forEach(tables => {
      tables.forEach(table => {
        if (
          table.table &&
          Array.isArray(table.table) &&
          table.table.length > 0
        ) {
          tableNames.push(table.table[0]);
        }
      });
    });

    return tableNames;
  };

  const getRoles = (): string[] => {
    const roles = new Set<string>();

    dataSourceTables?.forEach(tables => {
      tables.forEach(table => {
        const permissionTypes: PermissionType[] = [
          'select_permissions',
          'insert_permissions',
          'update_permissions',
          'delete_permissions',
        ];

        permissionTypes.forEach(permissionType => {
          const permissions = table[permissionType];
          permissions?.forEach(permission => {
            roles.add(permission.role);
          });
        });
      });
    });

    return Array.from(roles);
  };

  const getTablePermissions = (
    table: MetadataTable | undefined,
    type: PermissionType
  ) => {
    switch (type) {
      case 'select_permissions':
        return table?.select_permissions || [];
      case 'insert_permissions':
        return table?.insert_permissions || [];
      case 'update_permissions':
        return table?.update_permissions || [];
      case 'delete_permissions':
        return table?.delete_permissions || [];
      default:
        return [];
    }
  };

  return (
    <div className="pl-5 w-fit pt-5">
      <Helmet title="Permissions Summary | Hasura" />
      <BreadCrumbs dataSourceName={database} />
      <div className="my-5">
        <h2 className="font-extrabold text-xl pb-5">
          Permissions summary - {database}
        </h2>
      </div>
      <div className="pb-2 max-w-[200px]">
        <Select
          value={permissionType.find(
            option => option.value === selectedPermissionType
          )}
          options={permissionType}
          defaultValue={permissionType.find(
            option => option.value === 'select_permissions'
          )}
          onChange={(
            selectedOption: { value: string; label: string } | null
          ) => {
            if (selectedOption === null) return;
            setSelectedPermissionType(selectedOption.value as PermissionType);
          }}
        />
      </div>
      <table className="w-full border border-gray-300 rounded-md">
        <thead>
          <tr>
            <th className="px-4 py-2 border" />
            {getRoles().length ? (
              getRoles().map(role => (
                <th
                  key={role}
                  className="border py-2 px-4 font-bold min-w-24"
                  style={{ minWidth: '100px' }}
                >
                  {role}
                </th>
              ))
            ) : (
              <th
                className="border py-2 px-4 font-bold min-w-24"
                style={{ minWidth: '100px' }}
              >
                No Roles found
              </th>
            )}
          </tr>
        </thead>
        <tbody>
          {getTableNames().length ? (
            getTableNames().map((tableName, rowIndex) => (
              <tr key={tableName}>
                <th
                  className={`border py-2 px-4 min-w-24 ${
                    rowIndex === 0 ? 'border-t' : ''
                  }`}
                >
                  <span className="font-bold">{tableName}</span>
                </th>
                {getRoles().map((role, colIndex) => {
                  const permission = dataSourceTables
                    ?.map((tables): MetadataTable[] => tables.filter(Boolean))
                    .flatMap(table =>
                      getTablePermissions(
                        table.find(t => {
                          if (
                            t.table &&
                            Array.isArray(t.table) &&
                            t.table.length > 0
                          ) {
                            return t.table[0] === tableName;
                          }
                          return null;
                        }),
                        selectedPermissionType ?? 'select_permissions'
                      )
                    )
                    .filter(p => p !== null && typeof p === 'object');

                  const isChecked =
                    permission?.some(p => p.role === role) ?? false;

                  return (
                    <td
                      key={`${tableName}-${role}`}
                      className={`border p-2 min-w-24 ${
                        rowIndex === 0 ? 'border-t' : ''
                      } ${colIndex === 0 ? 'border-l' : ''}`}
                    >
                      {isChecked ? (
                        <span
                          onClick={() =>
                            dispatch(
                              _push(
                                getRoute().table(
                                  database,
                                  [tableName],
                                  'permissions'
                                )
                              )
                            )
                          }
                          role="img"
                          aria-label="crossmark"
                          className="text-green-500 flex justify-center group transition-transform transform hover:scale-110 cursor-pointer"
                        >
                          <FaCheck className="default-icon" />
                          <FaEdit className="hover-icon hidden group-hover:inline-block text-slate-400 ml-2" />
                        </span>
                      ) : (
                        <span
                          role="img"
                          aria-label="crossmark"
                          className="text-red-500 flex justify-center"
                        >
                          <FaTimes />
                        </span>
                      )}
                    </td>
                  );
                })}
              </tr>
            ))
          ) : (
            <th className="border py-2 px-4 min-w-24">
              <span className="font-bold">No tables found</span>
            </th>
          )}
        </tbody>
      </table>
    </div>
  );
};
