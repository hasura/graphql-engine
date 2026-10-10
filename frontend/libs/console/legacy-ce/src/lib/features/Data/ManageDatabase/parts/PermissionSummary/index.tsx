import { MetadataPermissionKey } from '@hasura/shared/types';
import { FaCheck, FaEdit, FaTimes } from 'react-icons/fa';
import { areTablesEqual } from '@hasura/metadata/helpers';
import { useNavigate } from 'react-router';
import { useDocumentTitle } from '@hasura/shared/hooks';
import {
  Breadcrumbs,
  CardedTable,
  IconButton,
  Select,
  Text,
} from '@hasura/shared/ui';
import { dataRoutes, getTableDisplayName } from '@hasura/shared/utils';
import { FaDatabase, FaTrash } from 'react-icons/fa6';
import { Flex, Heading, Skeleton } from '@radix-ui/themes';
import { useDataSourceContext } from '../../../context/DataSourceContext';
import usePermissionSummary from './usePermissionSummary';
import { CopyPermissionsButton } from './CopyPermissionsModal';

export const PermissionSummary = () => {
  useDocumentTitle('Permissions Summary | Hasura');

  const navigate = useNavigate();

  const { currentSource } = useDataSourceContext();
  const {
    isFetching,
    selectedPermissionType,
    setSelectedPermissionType,
    permissionTypes,
    handleDelete,
    isDeleting,
    roles,
  } = usePermissionSummary({
    source: currentSource,
  });

  const database = currentSource.name;
  const dataSourceTables = currentSource.tables;

  return (
    <div className="p-6">
      <Breadcrumbs
        items={[
          {
            title: 'Data',
            url: dataRoutes.manageDatabase,
          },
          {
            title: database,
            icon: <FaDatabase />,
            url: dataRoutes.manageDatabaseSource(database),
          },
        ]}
      />
      <div className="my-4">
        <Flex align="center" gap="4">
          <Heading size="4">Permissions summary - {database}</Heading>
          <CopyPermissionsButton
            source={currentSource}
            roles={roles}
            tables={dataSourceTables.map(({ table }) => table)}
          />
        </Flex>
      </div>
      <div className="pb-4 max-w-[200px]">
        <Skeleton loading={isFetching}>
          <Select
            value={selectedPermissionType}
            options={permissionTypes}
            onChange={(value) => {
              setSelectedPermissionType(value as MetadataPermissionKey);
            }}
          />
        </Skeleton>
      </div>
      <Skeleton loading={isFetching}>
        <CardedTable
          columns={
            roles.length
              ? [
                  '',
                  ...roles.map((role) => {
                    return (
                      <Flex key={role} align="center" gap="2">
                        <span>{role}</span>
                        <IconButton
                          mode="destructive"
                          variant="ghost"
                          radius="full"
                          title="Delete Permissions"
                          onClick={() => handleDelete(role)}
                          disabled={isDeleting}
                        >
                          <FaTrash />
                        </IconButton>
                      </Flex>
                    );
                  }),
                ]
              : ['No Role found']
          }
          data={
            dataSourceTables?.length
              ? dataSourceTables.map(({ table }, rowIndex) => [
                  <Text key={getTableDisplayName(table)} weight="bold">
                    {getTableDisplayName(table)}
                  </Text>,
                  ...roles.map((role) => {
                    const permission =
                      dataSourceTables?.find((t) =>
                        areTablesEqual(t.table, table),
                      )?.[selectedPermissionType ?? 'select_permissions'] ?? [];

                    const isChecked =
                      permission?.some(
                        (p: { role: string }) => p.role === role,
                      ) ?? false;

                    const key = `${rowIndex}-${role}`;

                    return isChecked ? (
                      <Flex
                        key={key}
                        align="center"
                        gap="2"
                        onClick={() =>
                          navigate(
                            dataRoutes.manageTable(
                              database,
                              table,
                              'permissions',
                            ),
                          )
                        }
                        role="img"
                        aria-label="crossmark"
                        className="group transition-transform transform hover:scale-110 cursor-pointer"
                      >
                        <IconButton
                          color="green"
                          size="1"
                          variant="ghost"
                          radius="full"
                        >
                          <FaCheck />
                        </IconButton>
                        <IconButton
                          color="gray"
                          size="1"
                          variant="ghost"
                          radius="full"
                        >
                          <FaEdit />
                        </IconButton>
                      </Flex>
                    ) : (
                      <IconButton
                        key={key}
                        color="red"
                        size="1"
                        variant="ghost"
                        radius="full"
                        className="cursor-none!"
                      >
                        <FaTimes />
                      </IconButton>
                    );
                  }),
                ])
              : [['No table found']]
          }
        />
      </Skeleton>
    </div>
  );
};
