import { OrderBy, TableColumn } from '@/features/DataSource';
import { Table } from '@/features/hasura-metadata-types';
import { Dialog } from '@/new-components/Dialog';
import { useConsoleForm } from '@/new-components/Form';
import React from 'react';
import { UseFormTrigger } from 'react-hook-form';
import { z } from 'zod';
import { RiPlayFill } from 'react-icons/ri';
import { FilterRows } from '../RunQuery/Filter';
import { SortRows } from '../RunQuery/Sort';
import { useTableColumns } from '../../hooks/useTableColumns';

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
    value: number | string | boolean | number[] | string[] | boolean[];
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

  const {
    methods: { trigger, watch },
    Form,
  } = useConsoleForm({
    schema,
    options: {
      defaultValues: {
        sorts: existingSorts,
        filters: existingFilters as any,
      },
    },
  });

  if (isLoading) return <>Loading...</>;

  if (!data) return <>Data not found!</>;

  const { columns, supportedOperators } = data;

  const handleSubmitQuery = async (
    filters: Schema['filters'],
    triggerValidation: UseFormTrigger<Schema>,
    sorts: Schema['sorts']
  ) => {
    if (await triggerValidation()) {
      onSubmit({
        filters: (filters ?? []).map(f => transformFilterValues(columns, f)),
        sorts: sorts ?? [],
      });
    }
  };

  const filters = watch('filters');
  const sorts = watch('sorts');

  const onSubmitHandler = () => handleSubmitQuery(filters, trigger, sorts);

  return (
    <div className="m-4">
      <Dialog hasBackdrop title="Query Data" onClose={onClose}>
        <>
          <Form onSubmit={() => {}}>
            <>
              <div className="p-4">
                <FilterRows
                  name="filters"
                  columns={columns}
                  operators={supportedOperators}
                  onRemove={() => onSubmitHandler()}
                />

                <hr className="my-4" />

                <SortRows
                  name="sorts"
                  columns={columns}
                  onRemove={() => onSubmitHandler()}
                />
              </div>
              <Dialog.Footer
                callToAction="Run Query"
                callToActionIconPosition="start"
                callToActionIcon={<RiPlayFill />}
                callToDeny="Cancel"
                onClose={onClose}
                onSubmit={() => onSubmitHandler()}
              />
            </>
          </Form>
        </>
      </Dialog>
    </div>
  );
};

QueryDialog.defaultProps = {
  filters: undefined,
  sorts: undefined,
};
