import React from 'react';
import {
  FaClone,
  FaEdit,
  FaExpand,
  FaTrash,
  FaCaretDown,
  FaCaretUp,
  FaSort,
} from 'react-icons/fa';
import { z } from 'zod';
import { Form, Checkbox } from '@/new-components/Form';
import { CardedTable } from '@/new-components/CardedTable';
import { GridIconButton } from '../../../../features/BrowseRows/components/DataGrid/GridIconButton';
import { UseRowsData } from '../../../../features/BrowseRows/components/DataGrid/types';

// Todo: add validation as forms is developed
const validationSchema = z.object({});

type IconSortingMapType = Record<'asc' | 'desc' | 'none', React.ReactElement>;

const IconSortingMap: IconSortingMapType = {
  desc: <FaCaretUp className="w-2 h-3 ml-1" />,
  asc: <FaCaretDown className="w-2 h-3 ml-1" />,
  none: <FaSort className="w-2 h-3 ml-1" />,
};

const HeaderAction = (
  columnName: string,
  onClick: (column: string) => void,
  Icon: React.ReactElement
) => {
  return (
    <div
      className="flex items-center cursor-pointer"
      onClick={() => onClick(columnName)}
    >
      <div>{columnName}</div>
      {Icon}
    </div>
  );
};

type DataGridProps = {
  data: UseRowsData;
  onColumnSortClick: (column: string) => void;
  isLoading: boolean;
};

const formatData = (rows: Record<string, string | number>[]) => {
  // Todo: hook up to react-query hooks
  const rowActions = [
    { icon: FaClone, type: 'clone', onClick: () => {} },
    { icon: FaEdit, type: 'edit', onClick: () => {} },
    { icon: FaExpand, type: 'expand', onClick: () => {} },
    { icon: FaTrash, type: 'delete', onClick: () => {} },
  ];

  const tableDataRows = rows?.map(objectRow => Object.values(objectRow)) ?? [];
  return tableDataRows.map((row, i) => {
    return [
      <div>
        {rowActions.map((action, index) => (
          <GridIconButton
            icon={<action.icon />}
            onClick={() => {}}
            rowIndex={index}
            type={action.type}
          />
        ))}
      </div>,
      <Checkbox
        name={`browse-rows-check-${i}`}
        options={[{ value: row[0], label: '' }]}
        noErrorPlaceholder
      />,
      ...row.map(value => <div>{value}</div>),
    ];
  });
};

export const DataGrid = (props: DataGridProps) => {
  const { data, onColumnSortClick, isLoading } = props;

  if (isLoading) return <>Loading data ...</>;

  const formattedRows = formatData(data.rows);
  return (
    <Form
      // Todo: Add form handling as needed
      schema={validationSchema}
      options={{
        defaultValues: undefined,
      }}
      onSubmit={() => {}}
      className="p-0"
    >
      {() => {
        return (
          <CardedTable.Table>
            <CardedTable.Header
              columns={
                data.columns
                  ? [
                      '', // Padding for first column with actions
                      <Checkbox
                        name="browse-rows-check-all"
                        options={[{ value: 'all', label: '' }]}
                        noErrorPlaceholder
                      />,

                      ...data.columns.map(column =>
                        HeaderAction(
                          column,
                          onColumnSortClick,
                          <div className="flex flex-col">
                            {column === data.orderBy?.column
                              ? IconSortingMap[data.orderBy?.type]
                              : IconSortingMap.none}
                          </div>
                        )
                      ),
                    ]
                  : []
              }
            />
            <CardedTable.Body data={formattedRows} />
          </CardedTable.Table>
        );
      }}
    </Form>
  );
};
