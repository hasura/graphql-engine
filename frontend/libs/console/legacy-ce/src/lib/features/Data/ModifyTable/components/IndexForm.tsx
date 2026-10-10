import { z } from 'zod';
import {
  CheckboxField,
  DelayedDialog,
  DialogFooter,
  getDialogPortalTarget,
  InputField,
  REACT_SELECT_FILTER_PROPS,
  ReactSelectField,
  useConsoleForm,
} from '@hasura/shared/ui';
import type { ColumnOption } from './ForeignKeyForm';

export const INDEX_TYPES = [
  'btree',
  'hash',
  'gin',
  'gist',
  'spgist',
  'brin',
] as const;

const schema = z.object({
  name: z.string().trim().min(1, { message: 'Index name is required' }),
  type: z.enum(INDEX_TYPES),
  columns: z
    .array(z.string())
    .min(1, { message: 'Select at least one column' }),
  unique: z.boolean(),
});

export type IndexFormValues = z.infer<typeof schema>;

export const emptyIndexFormValues: IndexFormValues = {
  name: '',
  type: 'btree',
  columns: [],
  unique: false,
};

const indexTypeOptions = INDEX_TYPES.map((t) => ({ value: t, label: t }));

export type IndexFormProps = {
  title: string;
  submitLabel: string;
  defaultValues: IndexFormValues;
  columnOptions: ColumnOption[];
  isPending?: boolean;
  onSubmit: (values: IndexFormValues) => void;
  onClose: () => void;
};

/**
 * Index editor dialog for ModifyTable. It owns its own form state, so
 * cancelling never touches the caller's values; the caller applies the result
 * via `onSubmit`.
 */
export const IndexForm = ({
  title,
  submitLabel,
  defaultValues,
  columnOptions,
  isPending = false,
  onSubmit,
  onClose,
}: IndexFormProps) => {
  const {
    Form,
    methods: { handleSubmit },
  } = useConsoleForm({ schema, options: { defaultValues } });

  return (
    <DelayedDialog
      size="md"
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
          ...REACT_SELECT_FILTER_PROPS,
          menuPortalTarget: getDialogPortalTarget(),
        };

        return (
          <div onSubmit={(e) => e.stopPropagation()}>
            <Form onSubmit={onSubmit}>
              <InputField
                name="name"
                label="Index name"
                fieldProps={{ placeholder: 'my_table_col_idx' }}
              />
              <ReactSelectField
                name="type"
                label="Index type"
                options={indexTypeOptions}
                selectProps={selectProps}
              />
              <ReactSelectField
                name="columns"
                label="Columns"
                placeholder="Select columns"
                multi
                options={columnOptions}
                selectProps={selectProps}
              />
              <CheckboxField name="unique">Unique index</CheckboxField>
            </Form>
          </div>
        );
      }}
    </DelayedDialog>
  );
};

export default IndexForm;
