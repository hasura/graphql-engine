import React, { useState } from 'react';
import isEqual from 'lodash/isEqual';
import {
  columnDataType,
  getDatabaseMethods,
  TableColumn,
  useDropColumn,
  useTableColumns,
  useTableForeignKeys,
  useTablePrimaryKey,
  useTableUniqueKeys,
} from '@hasura/metadata/data-source';
import { ModifyTableColumn, ModifyTableProps } from '../types';
import { EditTableColumnDialog } from './EditTableColumnDialog/EditTableColumnDialog';
import { TableColumnDescription } from './TableColumnDescription';
import { AddColumnDialog } from './AddColumnDialog';
import {
  Button,
  IndicatorCard,
  SkeletonList,
  useDestructiveConfirm,
} from '@hasura/shared/ui';
import { FaPlus } from 'react-icons/fa';
import { getErrorMessage } from '@hasura/shared/utils';

type TableColumnProps = ModifyTableProps;

export const TableColumns: React.FC<TableColumnProps> = (props) => {
  const {
    data: columns,
    isLoading,
    error,
  } = useTableColumns(
    {
      source: props.source,
      table: props.table.table,
    },
    {
      select: (result) => result.columns,
    },
  );

  const dbMethods = getDatabaseMethods(props.source.kind);
  const canListUniqueKeys = Boolean(dbMethods.introspection.getUniqueKeys);
  const canAddColumn = !props.isView && Boolean(dbMethods.modify?.addColumn);
  const [isAddDialogOpen, setIsAddDialogOpen] = useState(false);
  const { data: uniqueKeys = [] } = useTableUniqueKeys(
    { source: props.source, table: props.table.table },
    { enabled: canListUniqueKeys && !props.isView },
  );

  // Removing a column also drops the constraints that involve it; they are
  // passed along so the down migration can recreate them.
  const canDropColumn = !props.isView && Boolean(dbMethods.modify?.dropColumn);
  const { data: primaryKey } = useTablePrimaryKey(
    { source: props.source, table: props.table.table },
    {
      enabled: canDropColumn && Boolean(dbMethods.introspection.getPrimaryKey),
    },
  );
  // Same query as the Foreign Keys section, so this is served from its cache.
  const { data: foreignKeys = [] } = useTableForeignKeys({
    source: props.source,
    table: props.table.table,
  });
  const destructiveConfirm = useDestructiveConfirm();
  const { mutateAsync: dropColumn } = useDropColumn();

  const removeColumn = (column: ModifyTableColumn) =>
    destructiveConfirm({
      resourceName: column.name,
      resourceType: 'Column',
      onConfirm: async () => {
        try {
          await dropColumn({
            source: { name: props.source.name, kind: props.source.kind },
            table: props.table.table,
            column: {
              name: column.name,
              type: column.sqlType ?? columnDataType(column.dataType),
              nullable: Boolean(column.nullable),
              default: column.defaultValue ?? null,
            },
            primaryKey: primaryKey?.columns.includes(column.name)
              ? primaryKey
              : undefined,
            uniqueKeys: uniqueKeys.filter((uk) =>
              uk.columns.includes(column.name),
            ),
            foreignKeys: foreignKeys.filter(
              (fk) =>
                isEqual(fk.from.table, props.table.table) &&
                fk.from.columns.includes(column.name),
            ),
          });
          return true;
        } catch {
          return false;
        }
      },
    });

  const [isEditColumnFormActive, setIsEditColumnFormActive] = useState(false);
  const [selectedColumn, setSelectedColumn] = useState<ModifyTableColumn>();

  const resetDialogState = () => {
    setSelectedColumn(undefined);
    setIsEditColumnFormActive(false);
  };

  if (!columns && isLoading) {
    return <SkeletonList count={2} />;
  }

  // adding a "combinedStatus" as the loading variable before was not taking into account both queries and was not correctly showing a loader
  if (error) {
    return (
      <IndicatorCard status="negative">{getErrorMessage(error)}</IndicatorCard>
    );
  }

  const columnConfig = props.table.configuration?.column_config;

  return (
    <>
      {(columns ?? []).map((c: TableColumn) => (
        <TableColumnDescription
          column={{ ...c, config: columnConfig?.[c.name] }}
          key={c.name}
          onEdit={(column) => {
            setIsEditColumnFormActive(true);
            setSelectedColumn(column);
          }}
          onRemove={canDropColumn ? removeColumn : undefined}
        />
      ))}
      {isEditColumnFormActive && selectedColumn && (
        <EditTableColumnDialog
          {...props}
          column={selectedColumn}
          uniqueConstraintName={
            uniqueKeys.find(
              (uk) =>
                uk.columns.length === 1 &&
                uk.columns[0] === selectedColumn.name,
            )?.constraintName
          }
          onClose={resetDialogState}
        />
      )}
      {canAddColumn && (
        <div className="mt-2">
          <Button
            type="button"
            size="sm"
            mode="default"
            leftIcon={FaPlus}
            onClick={() => setIsAddDialogOpen(true)}
          >
            Add Column
          </Button>
        </div>
      )}
      {canAddColumn && isAddDialogOpen && (
        <AddColumnDialog
          source={props.source}
          table={props.table}
          onClose={() => setIsAddDialogOpen(false)}
        />
      )}
    </>
  );
};
