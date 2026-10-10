import {
  TableColumn,
  UseTableColumnsResult,
  columnDataType,
} from '@hasura/metadata/data-source';
import {
  Dialog,
  useConsoleForm,
  DialogFooter,
  Separator,
} from '@hasura/shared/ui';
import { FieldValues, UseFormTrigger } from 'react-hook-form';
import { z } from 'zod';
import { RiPlayFill } from 'react-icons/ri';
import { FilterRows } from '../RunQuery/Filter';
import { SortRows } from '../RunQuery/Sort';
import { OrderBy } from '@hasura/shared/types';

interface QueryDialogProps {
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
  tableColumns: UseTableColumnsResult | undefined;
}

export type FilterClause = { column: string; operator: string; value?: any };

const transformFilterValues = (
  columns: TableColumn[],
  filter: FilterClause,
) => {
  const column = columns.find(
    (x) => x.graphQLProperties?.name === filter.column,
  );

  if (!column) return filter;

  const dataType = column?.graphQLProperties?.scalarType ?? column.dataType;

  if (['boolean', 'Boolean'].includes(columnDataType(dataType))) return filter;

  if (['String', 'string'].includes(columnDataType(dataType))) return filter;

  return { ...filter, value: parseInt(filter.value, 10) };
};

const schema = z.object({
  filters: z.array(
    z.object({
      column: z.string().min(1, 'Column is required'),
      operator: z.string().min(1, 'Operator is required'),
      value: z.any(),
    }),
  ),
  sorts: z.array(
    z.object({
      column: z.string().min(1, 'Column is required'),
      type: z.literal('asc').or(z.literal('desc')),
    }),
  ),
});

type Schema = z.infer<typeof schema>;

export const QueryDialog = ({
  onClose,
  onSubmit,
  filters: existingFilters,
  sorts: existingSorts,
  tableColumns,
}: QueryDialogProps) => {
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

  const columns = tableColumns?.columns ?? [];
  const supportedOperators = tableColumns?.supportedOperators ?? [];

  const handleSubmitQuery = async (
    filters: Schema['filters'],
    triggerValidation: UseFormTrigger<FieldValues>,
    sorts: Schema['sorts'],
  ) => {
    if (await triggerValidation()) {
      onSubmit({
        filters: (filters ?? []).map((f) =>
          transformFilterValues(columns, f as FilterClause),
        ),
        sorts: (sorts as OrderBy[]) ?? [],
      });
    }
  };

  const filters = watch('filters');
  const sorts = watch('sorts');

  const onSubmitHandler = () =>
    handleSubmitQuery(filters, trigger as any, sorts);

  return (
    <div className="m-4">
      <Dialog title="Query Data" onClose={onClose}>
        <Form onSubmit={() => {}}>
          <div className="pt-3">
            <FilterRows
              name="filters"
              columns={columns}
              operators={supportedOperators}
            />
            <Separator size="4" className="my-4" />
            <SortRows name="sorts" columns={columns} />
          </div>
          <DialogFooter
            callToAction="Run Query"
            callToActionProps={{
              leftIcon: RiPlayFill,
            }}
            callToDeny="Cancel"
            onClose={onClose}
            onSubmit={() => onSubmitHandler()}
          />
        </Form>
      </Dialog>
    </div>
  );
};
