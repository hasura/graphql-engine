import { z } from 'zod';
import { useFieldArray } from 'react-hook-form';
import { FaPlus } from 'react-icons/fa';
import {
  Button,
  DelayedDialog,
  DialogFooter,
  ErrorCard,
  filterOptionByLabel,
  getDialogPortalTarget,
  IconButtonDelete,
  RadioGroupField,
  ReactSelectField,
  Table,
  Text,
  useConsoleForm,
} from '@hasura/shared/ui';
import type { Source, Table as HasuraTable } from '@hasura/shared/types';
import {
  createTableSelectOption,
  getDatabaseMethods,
  TableFkRelationships,
  useTableColumns,
  ViolationAction,
} from '@hasura/metadata/data-source';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import { useMetadata } from '@hasura/metadata/api';

export const fkFormSchema = z.object({
  referenceTable: z.unknown(),
  columnMappings: z
    .array(
      z.object({
        from: z.string().min(1, { message: 'Required' }),
        to: z.string().min(1, { message: 'Required' }),
      }),
    )
    .min(1),
  onUpdate: z.string(),
  onDelete: z.string(),
});

export type FkFormValues = z.infer<typeof fkFormSchema>;

export const getDefaultViolationAction = (source: Source): ViolationAction => {
  const actions =
    getDatabaseMethods(source.kind).config?.getViolationActions?.() ?? [];
  return (actions.includes('restrict') ? 'restrict' : actions[0]) ?? 'restrict';
};

export const emptyFkFormValues = (source: Source): FkFormValues => {
  const action = getDefaultViolationAction(source);
  return {
    referenceTable: undefined,
    columnMappings: [{ from: '', to: '' }],
    onUpdate: action,
    onDelete: action,
  };
};

/** Snapshot an existing foreign key into editable form values. */
export const fkToFormValues = (
  fk: TableFkRelationships,
  source: Source,
): FkFormValues => {
  const action = getDefaultViolationAction(source);
  return {
    referenceTable: fk.to.table,
    columnMappings: fk.from.columns.map((from, i) => ({
      from,
      to: fk.to.columns[i] ?? '',
    })),
    onUpdate: fk.onUpdate ?? action,
    onDelete: fk.onDelete ?? action,
  };
};

export type ColumnOption = { value: string; label: string };

export type ForeignKeyFormProps = {
  source: Source;
  /** The existing table the foreign key is defined on; its columns are
   *  introspected for the "From column" options. */
  table?: HasuraTable;
  /** "From column" options for a table that doesn't exist yet (e.g. AddTable).
   *  When provided, `table` is not introspected. */
  ownColumnOptions?: ColumnOption[];
  title: string;
  defaultValues: FkFormValues;
  submitLabel: string;
  isPending?: boolean;
  error?: unknown;
  onSubmit: (values: FkFormValues) => void;
  onClose: () => void;
};

/**
 * Shared foreign-key editor dialog used for create and edit, in both
 * ModifyTable and AddTable. It owns its own form state, so cancelling never
 * touches the caller's values; the caller applies the result via `onSubmit`.
 */
export const ForeignKeyForm = ({
  source,
  table,
  ownColumnOptions: ownColumnOptionsProp,
  title,
  defaultValues,
  submitLabel,
  isPending = false,
  onSubmit,
  onClose,
  error,
}: ForeignKeyFormProps) => {
  const {
    Form,
    methods: { control, watch, handleSubmit },
  } = useConsoleForm({
    schema: fkFormSchema,
    options: { defaultValues },
  });

  const mappings = useFieldArray({ control, name: 'columnMappings' });

  const { data: ownColumns } = useTableColumns({
    source,
    table: ownColumnOptionsProp ? undefined : table,
  });
  const ownColumnOptions =
    ownColumnOptionsProp ??
    (ownColumns?.columns ?? []).map((c) => ({
      value: c.name,
      label: c.name,
    }));

  const referenceTable = watch('referenceTable') as HasuraTable;
  const { data: refColumns } = useTableColumns({
    source,
    table: referenceTable,
  });
  const refColumnOptions = (refColumns?.columns ?? []).map((c) => ({
    value: c.name,
    label: c.name,
  }));

  const { data: tables = [] } = useMetadata(
    MetadataSelectors.getTables(source.name),
  );
  const referenceTableOptions = tables.map((t) =>
    createTableSelectOption(t.table),
  );

  const violationActionOptions = (
    getDatabaseMethods(source.kind).config?.getViolationActions?.() ?? []
  ).map((v) => ({ value: v, label: v.toUpperCase() }));

  return (
    <DelayedDialog
      size="lg"
      title={title}
      onClose={onClose}
      footer={
        <DialogFooter
          onSubmit={() => {
            handleSubmit(onSubmit)();
          }}
          isLoading={isPending}
          onClose={onClose}
          callToDeny="Cancel"
          callToAction={submitLabel}
        />
      }
    >
      {/* The dialog is portalled, but React events still bubble through the
          component tree: keep this form's submit from reaching a parent form. */}
      {() => {
        const selectProps = {
          isSearchable: true,
          filterOption: filterOptionByLabel,
          menuPortalTarget: getDialogPortalTarget(),
        };

        return (
          <div onSubmit={(e) => e.stopPropagation()}>
            <Form onSubmit={onSubmit}>
              <ReactSelectField
                name="referenceTable"
                label="Reference table"
                placeholder="Select a table"
                options={referenceTableOptions}
                selectProps={selectProps}
              />
              {mappings.fields.length > 0 && (
                <Table.Root size="1">
                  <Table.Header>
                    <Table.Row>
                      <Table.RowHeaderCell>
                        <Text weight="bold">From column</Text>
                      </Table.RowHeaderCell>
                      <Table.RowHeaderCell>
                        <Text weight="bold">To column</Text>
                      </Table.RowHeaderCell>
                      <Table.RowHeaderCell></Table.RowHeaderCell>
                    </Table.Row>
                  </Table.Header>
                  <Table.Body>
                    {mappings.fields.map((field, index) => (
                      <Table.Row key={field.id}>
                        <Table.Cell>
                          <ReactSelectField
                            name={`columnMappings.${index}.from`}
                            placeholder="Select a column"
                            options={ownColumnOptions}
                            selectProps={selectProps}
                            noErrorPlaceholder
                          />
                        </Table.Cell>
                        <Table.Cell>
                          <ReactSelectField
                            name={`columnMappings.${index}.to`}
                            placeholder="Select a column"
                            options={refColumnOptions}
                            selectProps={selectProps}
                            noErrorPlaceholder
                          />
                        </Table.Cell>
                        <Table.Cell width="32px">
                          <div className="pt-2">
                            <IconButtonDelete
                              type="button"
                              aria-label={`Remove column mapping ${index + 1}`}
                              disabled={mappings.fields.length === 1}
                              onClick={() => mappings.remove(index)}
                            />
                          </div>
                        </Table.Cell>
                      </Table.Row>
                    ))}
                  </Table.Body>
                </Table.Root>
              )}
              <div className="my-2">
                <Button
                  type="button"
                  size="1"
                  mode="default"
                  leftIcon={FaPlus}
                  onClick={() => mappings.append({ from: '', to: '' })}
                >
                  Add column mapping
                </Button>
              </div>
              <RadioGroupField
                name="onUpdate"
                label="On update"
                options={violationActionOptions}
                orientation="horizontal"
                noErrorPlaceholder
              />
              <div className="my-2">
                <RadioGroupField
                  name="onDelete"
                  label="On delete"
                  options={violationActionOptions}
                  orientation="horizontal"
                  noErrorPlaceholder
                />
              </div>
            </Form>
            {error ? (
              <ErrorCard
                headline="Modifying foreign key failed"
                error={error}
              />
            ) : null}
          </div>
        );
      }}
    </DelayedDialog>
  );
};

export default ForeignKeyForm;
