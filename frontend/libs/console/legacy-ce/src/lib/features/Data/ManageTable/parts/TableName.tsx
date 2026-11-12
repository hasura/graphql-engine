import {
  Badge,
  DestructiveDialogCascade,
  useCascadeDialog,
  DropdownButton,
  DropdownMenu,
  getDestructiveDescription,
  hasuraToast,
  Text,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';
import React, { useState } from 'react';
import { FaTable } from 'react-icons/fa';
import { Source, Table } from '@hasura/shared/types';
import { CreateRestEndpoint } from '../../../ApiExplorer/components/Rest/CreateRestEndpoints';
import { useErrorNotification, useUntrackTable } from '@hasura/metadata/api';
import { useNavigate } from 'react-router';
import { dataRoutes, extractTableInfo } from '@hasura/shared/utils';
import { areTablesEqual, isNativeDriver } from '@hasura/metadata/helpers';
import { getDatabaseMethods, useDropTable } from '@hasura/metadata/data-source';
import { SchemaDropdown } from '../../ManageDatabase/parts';
import { TryOperation } from './TryOperation/TryOperation';
import { RenameTableDialog } from './RenameTableDialog';

export const TableName: React.FC<{
  source: Source;
  table: Table;
  tableName: string;
}> = ({ source, table, tableName }) => {
  const navigate = useNavigate();
  const { mutateAsync: untrackTable, isPending: isUntracking } =
    useUntrackTable();
  const { mutate: dropTable, isPending: isDropping } = useDropTable();
  const showErrorNotification = useErrorNotification();
  const destructiveDialog = useCascadeDialog<'untrack' | 'delete'>();
  const [isRenameDialogOpen, setIsRenameDialogOpen] = useState(false);

  const dbMethods = getDatabaseMethods(source.kind);
  const metadataTable = source.tables?.find((t) =>
    areTablesEqual(t.table, table),
  );
  const rawTableName =
    extractTableInfo(table)?.name || tableName.split('.').pop() || tableName;

  const handleUntrack = (cascade: boolean) => {
    destructiveDialog.clearError();
    return untrackTable({ source, table, cascade })
      .then(() => {
        hasuraToast({
          type: 'success',
          title: 'Successfully untracked table',
        });
        destructiveDialog.close();
        navigate(dataRoutes.manageDatabaseSource(source.name));
      })
      .catch((err) =>
        destructiveDialog.handleError(err, (error) =>
          showErrorNotification({
            title: 'Error while untracking table',
            error,
          }),
        ),
      );
  };

  const handleDelete = (cascade: boolean) => {
    destructiveDialog.clearError();
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
        onError: (err) =>
          destructiveDialog.handleError(err, (error) =>
            showErrorNotification({
              title: 'Dropping table failed',
              error,
            }),
          ),
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
          items={(dbMethods.modify?.changeTableName
            ? [
                <DropdownMenu.Item
                  key="rename"
                  onSelect={() => setIsRenameDialogOpen(true)}
                >
                  Rename
                </DropdownMenu.Item>,
              ]
            : []
          )
            .concat([
              <DropdownMenu.Item
                key="untrack"
                onSelect={() => destructiveDialog.open('untrack')}
              >
                Untrack
              </DropdownMenu.Item>,
            ])
            .concat(
              dbMethods.modify?.dropTable
                ? [
                    <DropdownMenu.Item
                      key="delete"
                      color="red"
                      onSelect={() => destructiveDialog.open('delete')}
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
        {metadataTable && (
          <TryOperation source={source} metadataTable={metadataTable} />
        )}
        {isNativeDriver(source.kind) && (
          <CreateRestEndpoint
            tableName={rawTableName}
            dataSourceName={source.name}
            table={table}
          />
        )}
      </Flex>
      {isRenameDialogOpen && (
        <RenameTableDialog
          source={source}
          table={table}
          currentName={rawTableName}
          onClose={() => setIsRenameDialogOpen(false)}
        />
      )}
      {destructiveDialog.openFor && (
        <DestructiveDialogCascade
          {...destructiveDialog.dialogProps}
          title={
            destructiveDialog.openFor === 'untrack'
              ? 'Untrack table'
              : 'Delete table'
          }
          loading={
            destructiveDialog.openFor === 'untrack' ? isUntracking : isDropping
          }
          onConfirm={
            destructiveDialog.openFor === 'untrack'
              ? handleUntrack
              : handleDelete
          }
        >
          {getDestructiveDescription({
            destroyTerm: destructiveDialog.openFor,
            resourceName: tableName,
            resourceType: 'table',
          })}
        </DestructiveDialogCascade>
      )}
    </>
  );
};
