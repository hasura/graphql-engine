import { OrderBy, TableColumn } from '@/features/DataSource';
import { Table } from '@/features/MetadataAPI';
import { Dialog } from '@/new-components/Dialog';
import { UpdatedForm } from '@/new-components/Form';
import React from 'react';
import { UseFormTrigger } from 'react-hook-form';
import { z } from 'zod';
import { FilterRows } from '../RunQuery/Filter';
import { SortRows } from '../RunQuery/Sort';
import { useTableColumns } from './useTableColumns';

interface QueryDialogProps {
  table: Table;
  dataSourceName: string;
  onClose: () => void;
  onSubmit: (values: {
    filters: {
      column: string;
      operator: string;
      value?: number | string | boolean;
    }[];
    sorts: OrderBy[];
  }) => void;
  filters?: {
    column: string;
    operator: string;
    value: number | string | boolean;
  }[];
  sorts?: OrderBy[];
}

export type FilterClause = { column: string; operator: string; value?: any };

const transformFilterValues = (
  columns: TableColumn[],
  filter: FilterClause
) => {
  const column = columns.find(x => x.graphQLProperties?.name === filter.column);

  if (!column) return filter;

  const dataType = column?.graphQLProperties?.scalarType ?? column.dataType;

  if (['boolean', 'Boolean'].includes(dataType)) return filter;

  if (['String', 'string'].includes(dataType)) return filter;

  return { ...filter, value: parseInt(filter.value, 10) };
};

const schema = z.object({
  filters: z
    .array(
      z.object({
        column: z.string(),
        operator: z.string(),
        value: z.any(),
      })
    )
    .optional(),
  sorts: z
    .array(
      z.object({
        column: z.string(),
        type: z.literal('asc').or(z.literal('desc')),
      })
    )
    .optional(),
});

type Schema = z.infer<typeof schema>;

export const QueryDialog = ({
  onClose,
  table,
  dataSourceName,
  onSubmit,
  filters: existingFilters,
  sorts: existingSorts,
}: QueryDialogProps) => {
  const { data, isLoading } = useTableColumns({ table, dataSourceName });

  if (isLoading) return <>Loading...</>;

  if (!data) return <>Data not found!</>;

  const { columns, supportedOperators } = data;

  const handleSubmitQuery = async (
    filters: Schema['filters'],
    trigger: UseFormTrigger<Schema>,
    sorts: Schema['sorts']
  ) => {
    if (await trigger()) {
      onSubmit({
        filters: (filters ?? []).map(f => transformFilterValues(columns, f)),
        sorts: sorts ?? [],
      });
    }
  };

  return (
    <div className="m-4">
      <Dialog hasBackdrop title="Query Data" onClose={onClose}>
        <>
          <UpdatedForm
            schema={schema}
            options={{
              defaultValues: {
                sorts: existingSorts,
                filters: existingFilters as any,
              },
            }}
            onSubmit={() => {}}
          >
            {({ trigger, watch }) => {
              const filters = watch('filters');
              const sorts = watch('sorts');

              return (
                <>
                  <div className="p-4">
                    <FilterRows
                      name="filters"
                      columns={columns}
                      operators={supportedOperators}
                    />

                    <hr className="my-4" />

                    <SortRows name="sorts" columns={columns} />
                  </div>
                  <Dialog.Footer
                    callToAction="Run Query"
                    onClose={onClose}
                    onSubmit={() => handleSubmitQuery(filters, trigger, sorts)}
                  />
                </>
              );
            }}
          </UpdatedForm>
        </>
      </Dialog>
    </div>
  );
};
