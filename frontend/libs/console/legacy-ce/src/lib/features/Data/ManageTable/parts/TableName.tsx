import {
  Badge,
  DestructiveDialogCascade,
  DropdownButton,
  DropdownMenu,
  getDestructiveDescription,
  hasuraToast,
  showErrorNotification,
  Text,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';
import React, { useState } from 'react';
import { FaTable } from 'react-icons/fa';
import { Source, Table } from '@hasura/shared/types';
import { CreateRestEndpoint } from '../../../ApiExplorer/components/Rest/CreateRestEndpoints';
import { useUntrackTable } from '@hasura/metadata/api';
import { useNavigate } from 'react-router';
import { dataRoutes, extractTableInfo } from '@hasura/shared/utils';
import { isNativeDriver } from '@hasura/metadata/helpers';
import { getDatabaseMethods, useDropTable } from '@hasura/metadata/data-source';
import { SchemaDropdown } from '../../ManageDatabase/parts';

export const TableName: React.FC<{
  source: Source;
  table: Table;
  tableName: string;
}> = ({ source, table, tableName }) => {
  const navigate = useNavigate();
  const { mutateAsync: untrackTable, isPending: isUntracking } =
    useUntrackTable();
  const { mutate: dropTable, isPending: isDropping } = useDropTable();
  const [openDestructiveDialog, setOpenDestructiveDialog] = useState<
    'none' | 'untrack' | 'delete'
  >('none');

  const dbMethods = getDatabaseMethods(source.kind);

  const handleUntrack = (cascade: boolean) => {
    return untrackTable({ source, table, cascade })
      .then(() => {
        hasuraToast({
          type: 'success',
          title: 'Successfully untracked table',
        });
        setOpenDestructiveDialog('none');
        navigate(dataRoutes.manageDatabaseSource(source.name));
      })
      .catch((err) => {
        showErrorNotification({
          title: 'Error while untracking table',
          error: err,
        });
      });
  };

  const handleDelete = (cascade: boolean) => {
    return dropTable(
      {
        source,
        table,
        cascade,
      },
      {
        onSuccess: () => {
          navigate(dataRoutes.manageDatabaseSource(source.name));
        },
      },
    );
  };

  return (
    <>
      <Flex align="center" gap="3" className="mb-3">
        <SchemaDropdown source={source} />
        <DropdownButton
          mode="default"
          variant="ghost"
          leftIcon={FaTable}
          disabled={isUntracking || isDropping}
          items={[
            <DropdownMenu.Item
              key="untrack"
              onSelect={() => setOpenDestructiveDialog('untrack')}
            >
              Untrack
            </DropdownMenu.Item>,
          ].concat(
            dbMethods.modify?.dropTable
              ? [
                  <DropdownMenu.Item
                    key="delete"
                    color="red"
                    onSelect={() => setOpenDestructiveDialog('delete')}
                  >
                    Delete
                  </DropdownMenu.Item>,
                ]
              : [],
          )}
        >
          <Text weight="bold">{tableName}</Text>
        </DropdownButton>
        <div>
          <Badge color="green">Tracked</Badge>
        </div>
        {isNativeDriver(source.kind) && (
          <CreateRestEndpoint
            tableName={
              extractTableInfo(table)?.name ||
              tableName.split('.').pop() ||
              tableName
            }
            dataSourceName={source.name}
            table={table}
          />
        )}
      </Flex>
      {openDestructiveDialog !== 'none' && (
        <DestructiveDialogCascade
          title={
            openDestructiveDialog === 'untrack'
              ? 'Untrack table'
              : 'Delete table'
          }
          onClose={() => setOpenDestructiveDialog('none')}
          onConfirm={
            openDestructiveDialog === 'untrack' ? handleUntrack : handleDelete
          }
        >
          {getDestructiveDescription({
            destroyTerm: openDestructiveDialog,
            resourceName: tableName,
            resourceType: 'table',
          })}
        </DestructiveDialogCascade>
      )}
    </>
  );
};
