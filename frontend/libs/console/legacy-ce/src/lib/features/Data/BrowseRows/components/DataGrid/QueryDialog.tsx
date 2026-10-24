import { Operator, UseTableColumnsResult } from '@hasura/metadata/data-source';
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
import {
  getFilterValueKind,
  toFilterValue,
} from '../RunQuery/Filter/FilterRow';
import { SortRows } from '../RunQuery/Sort';
import { OrderBy } from '@hasura/shared/types';
import {
  FilterClause,
  isIgnoredFilter,
  transformFilterValues,
} from './QueryDialog.utils';

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
  /** the filter operators each column supports, by column name */
  columnOperators?: Record<string, Operator[]>;
  /** the allowed values of the columns that reference an enum table */
  enumColumnValues?: Record<string, string[]>;
}

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
  columnOperators,
  enumColumnValues,
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
        filters: (filters ?? [])
          .map((filter) => {
            // e.g. parse a JSON document, or format a date for the column
            const column = columns.find((c) => c.name === filter.column);
            const operator = (
              columnOperators?.[filter.column] ?? supportedOperators
            ).find((op) => op.value === filter.operator);
            const kind = getFilterValueKind(filter.operator, {
              operator,
              column,
            });
            return {
              ...filter,
              value: toFilterValue(filter.value, kind, column),
            };
          })
          .filter((f) => !isIgnoredFilter(columns, f as FilterClause))
          .map((f) => transformFilterValues(columns, f as FilterClause)),
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
              columnOperators={columnOperators}
              enumColumnValues={enumColumnValues}
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
