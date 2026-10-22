import {
  TableColumn,
  UseTableColumnsResult,
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

// Only these console data types carry a numeric filter value.
const NUMERIC_CONSOLE_DATA_TYPES: TableColumn['consoleDataType'][] = [
  'integer',
  'number',
  'float',
];

export const transformFilterValues = (
  columns: TableColumn[],
  filter: FilterClause,
) => {
  const column = columns.find(
    (x) => x.graphQLProperties?.name === filter.column,
  );

  if (!column) return filter;

  // Coerce to a number only for numeric columns. Every other scalar type must
  // be forwarded untouched: `parseInt` truncates or replaces the value for
  // uuid/timestamptz/date/json/custom scalars (e.g. the uuid
  // `31b789b0-acfe-43a8-bdaf-561313e8f82e` became `31`), which silently
  // filtered on the wrong rows. It also truncated float values (`1.5` -> `1`).
  if (!NUMERIC_CONSOLE_DATA_TYPES.includes(column.consoleDataType)) {
    return filter;
  }

  if (
    filter.value === '' ||
    filter.value === null ||
    filter.value === undefined
  ) {
    return filter;
  }

  // `Number` (not `parseInt`) preserves the fractional part of float columns.
  return { ...filter, value: Number(filter.value) };
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
        <Form
          onSubmit={() => {
            /* noop */
          }}
        >
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
