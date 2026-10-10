import { z } from 'zod';
import {
  DelayedDialog,
  DialogFooter,
  ErrorCard,
  getDialogPortalTarget,
  REACT_SELECT_FILTER_PROPS,
  ReactSelectField,
  useConsoleForm,
} from '@hasura/shared/ui';
import type { ColumnOption } from './ForeignKeyForm';

const schema = z.object({
  columns: z
    .array(z.string())
    .min(1, { message: 'Select at least one column' }),
});

export type PrimaryKeyFormValues = z.infer<typeof schema>;

export type PrimaryKeyFormProps = {
  title: string;
  submitLabel: string;
  defaultValues: PrimaryKeyFormValues;
  columnOptions: ColumnOption[];
  isPending?: boolean;
  error?: unknown;
  onSubmit: (values: PrimaryKeyFormValues) => void;
  onClose: () => void;
};

/**
 * Primary-key editor dialog used for create and edit in ModifyTable. It owns
 * its own form state, so cancelling never touches the caller's values; the
 * caller applies the result via `onSubmit`.
 */
export const PrimaryKeyForm = ({
  title,
  submitLabel,
  defaultValues,
  columnOptions,
  isPending = false,
  error,
  onSubmit,
  onClose,
}: PrimaryKeyFormProps) => {
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
      {() => (
        <div onSubmit={(e) => e.stopPropagation()}>
          <Form onSubmit={onSubmit}>
            <ReactSelectField
              name="columns"
              label="Primary key columns"
              placeholder="Select columns"
              multi
              options={columnOptions}
              selectProps={{
                ...REACT_SELECT_FILTER_PROPS,
                menuPortalTarget: getDialogPortalTarget(),
              }}
            />
          </Form>
          {error ? (
            <ErrorCard headline="Modifying primary key failed" error={error} />
          ) : null}
        </div>
      )}
    </DelayedDialog>
  );
};

export default PrimaryKeyForm;
