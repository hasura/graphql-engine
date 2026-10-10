import { z } from 'zod';
import {
  DelayedDialog,
  DialogFooter,
  getDialogPortalTarget,
  InputField,
  REACT_SELECT_FILTER_PROPS,
  ReactSelectField,
  useConsoleForm,
} from '@hasura/shared/ui';
import type { ColumnOption } from './ForeignKeyForm';

const baseSchema = z.object({
  name: z.string().trim(),
  columns: z
    .array(z.string())
    .min(1, { message: 'Select at least one column' }),
});

// With a name field the constraint name is required.
const namedSchema = baseSchema.extend({
  name: z.string().trim().min(1, { message: 'Constraint name is required' }),
});

export type UniqueKeyFormValues = z.infer<typeof baseSchema>;

export const emptyUniqueKeyFormValues: UniqueKeyFormValues = {
  name: '',
  columns: [],
};

export type UniqueKeyFormProps = {
  title: string;
  submitLabel: string;
  defaultValues: UniqueKeyFormValues;
  columnOptions: ColumnOption[];
  /** Show (and require) a constraint name. AddTable leaves naming to the
   *  database, so it hides the field. */
  withName?: boolean;
  isPending?: boolean;
  onSubmit: (values: UniqueKeyFormValues) => void;
  onClose: () => void;
};

/**
 * Shared unique-key editor dialog for AddTable and ModifyTable. It owns its own
 * form state, so cancelling never touches the caller's values; the caller
 * applies the result via `onSubmit`.
 */
export const UniqueKeyForm = ({
  title,
  submitLabel,
  defaultValues,
  columnOptions,
  withName = true,
  isPending = false,
  onSubmit,
  onClose,
}: UniqueKeyFormProps) => {
  const {
    Form,
    methods: { handleSubmit },
  } = useConsoleForm({
    schema: withName ? namedSchema : baseSchema,
    options: { defaultValues },
  });

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
      {() => (
        <div onSubmit={(e) => e.stopPropagation()}>
          <Form onSubmit={onSubmit}>
            {withName && (
              <InputField
                name="name"
                label="Constraint name"
                fieldProps={{ placeholder: 'my_table_col_key' }}
              />
            )}
            <ReactSelectField
              name="columns"
              label="Columns"
              placeholder="Select columns"
              multi
              options={columnOptions}
              selectProps={{
                ...REACT_SELECT_FILTER_PROPS,
                menuPortalTarget: getDialogPortalTarget(),
              }}
            />
          </Form>
        </div>
      )}
    </DelayedDialog>
  );
};

export default UniqueKeyForm;
