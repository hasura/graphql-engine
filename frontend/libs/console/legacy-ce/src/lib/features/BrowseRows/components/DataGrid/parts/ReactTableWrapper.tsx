import { TableRow } from '@/features/DataSource';
import { CardedTable } from '@/new-components/CardedTable';
import {
  FaCaretDown,
  FaCaretUp,
  FaExternalLinkAlt,
  FaLink,
} from 'react-icons/fa';
import {
  ColumnDef,
  ColumnSort,
  createColumnHelper,
  flexRender,
  getCoreRowModel,
  useReactTable,
} from '@tanstack/react-table';
import React, { useMemo } from 'react';
import clsx from 'clsx';
import { Relationship } from '@/features/DatabaseRelationships';

interface ReactTableWrapperProps {
  rows: TableRow[];

  sort?: {
    sorting: ColumnSort[];
    setSorting: React.Dispatch<React.SetStateAction<ColumnSort[]>>;
  };

  relationships?: {
    allRelationships: Relationship[];
    onClick: (props: { relationship: Relationship; rowData: TableRow }) => void;
    onClose: (relationshipName: string) => void;
    activeRelationships?: string[];
  };
}

export const ReactTableWrapper = (props: ReactTableWrapperProps) => {
  const { rows, sort, relationships } = props;

  const columns = Object.keys(rows?.[0] ?? []);

  const columnHelper = createColumnHelper<TableRow>();

  const tableColumns = columns.map(column =>
    columnHelper.accessor(row => row[column], {
      id: column,
      cell: (info: any) => info.getValue() ?? '',
      header: () => <span key={column}>{column}</span>,
    })
  );

  const relationshipColumns = useMemo(
    () =>
      (relationships?.allRelationships ?? []).reduce<
        ColumnDef<TableRow, TableRow>[]
      >((cols, relationship) => {
        if (relationship.type !== 'localRelationship') return cols;

        return [
          ...cols,
          columnHelper.accessor(row => row, {
            id: relationship.name,
            cell: (info: any) =>
              (relationships?.activeRelationships ?? []).includes(
                relationship.name
              ) ? (
                <span
                  className="text-red-600 cursor-pointer"
                  onClick={() => relationships?.onClose(relationship.name)}
                  data-testid={`@view-relationship-${relationship.name}-goto-link`}
                >
                  <FaExternalLinkAlt />
                </span>
              ) : (
                <a
                  onClick={() => {
                    relationships?.onClick({
                      relationship,
                      rowData: info.row.original,
                    });
                  }}
                  className="cursor-pointer"
                  data-testid={`@view-relationship-${relationship.name}`}
                >
                  View
                </a>
              ),
            header: () => (
              <span
                className="flex items-center gap-0.5"
                key={relationship.name}
              >
                <FaLink />
                {relationship.name}
              </span>
            ),
          }),
        ];
      }, []),
    [columnHelper, relationships]
  );

  const relationshipNames =
    relationships?.allRelationships.map(rel => rel.name) ?? [];

  const ReactTable = useReactTable<TableRow>({
    data: rows,
    columns: [...tableColumns, ...relationshipColumns],
    getCoreRowModel: getCoreRowModel(),
    state: {
      sorting: sort?.sorting,
    },
    onSortingChange: sort?.setSorting,
    manualPagination: true,
  });

  if (!rows.length) return <div>No rows Available</div>;

  return (
    <CardedTable.Table
      className="overflow-y-auto"
      style={{ maxHeight: '65vh' }}
    >
      <CardedTable.TableHead>
        {ReactTable.getHeaderGroups().map((headerGroup, id) => (
          <CardedTable.TableHeadRow key={`${headerGroup.id}-${id}`}>
            {headerGroup.headers.map((header, i) => (
              <CardedTable.TableHeadCell key={`cell-${i}`}>
                {header.isPlaceholder ? null : (
                  <div
                    onClick={header.column.getToggleSortingHandler()}
                    className={clsx(
                      relationshipNames.includes(header.id)
                        ? 'pointer-events-none'
                        : 'pointer-events-auto',
                      header.column.getCanSort()
                        ? 'cursor-pointer select-none flex gap-4 items-center'
                        : ''
                    )}
                    key={`header-item-${header.id}`}
                  >
                    {flexRender(
                      header.column.columnDef.header,
                      header.getContext()
                    )}
                    {!relationshipNames.includes(header.id) &&
                      ({
                        asc: <FaCaretUp />,
                        desc: <FaCaretDown />,
                      }[header.column.getIsSorted() as string] ?? (
                        <span className="flex flex-col">
                          <FaCaretUp />
                          <FaCaretDown style={{ marginTop: '-5px' }} />
                        </span>
                      ))}
                  </div>
                )}
              </CardedTable.TableHeadCell>
            ))}
          </CardedTable.TableHeadRow>
        ))}
      </CardedTable.TableHead>

      <CardedTable.TableBody>
        {ReactTable.getRowModel().rows.map(row => (
          <CardedTable.TableBodyRow
            key={row.id}
            data-testid={`@table-row-${row.id}`}
          >
            {row.getVisibleCells().map((cell, i) => (
              <CardedTable.TableBodyCell
                key={i}
                data-testid={`@table-cell-${row.id}-${i}`}
              >
                {flexRender(cell.column.columnDef.cell, cell.getContext())}
              </CardedTable.TableBodyCell>
            ))}
          </CardedTable.TableBodyRow>
        ))}
      </CardedTable.TableBody>
    </CardedTable.Table>
  );
};

ReactTableWrapper.defaultProps = {
  sort: undefined,
};
