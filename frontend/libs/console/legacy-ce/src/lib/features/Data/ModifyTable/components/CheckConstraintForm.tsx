import { z } from 'zod';
import {
  CodeEditorField,
  Dialog,
  DialogFooter,
  InputField,
  useConsoleForm,
} from '@hasura/shared/ui';

export const checkConstraintFormSchema = z.object({
  name: z.string().trim().min(1, { message: 'Constraint name is required' }),
  check: z.string().trim().min(1, { message: 'Check expression is required' }),
});

export type CheckConstraintFormValues = z.infer<
  typeof checkConstraintFormSchema
>;

export const emptyCheckConstraintFormValues: CheckConstraintFormValues = {
  name: '',
  check: '',
};

export type CheckConstraintFormProps = {
  title: string;
  submitLabel: string;
  defaultValues: CheckConstraintFormValues;
  isPending?: boolean;
  onSubmit: (values: CheckConstraintFormValues) => void;
  onClose: () => void;
};

/**
 * Shared check-constraint editor dialog for AddTable and ModifyTable. It owns
 * its own form state, so cancelling never touches the caller's values; the
 * caller applies the result via `onSubmit`.
 */
export const CheckConstraintForm = ({
  title,
  submitLabel,
  defaultValues,
  isPending = false,
  onSubmit,
  onClose,
}: CheckConstraintFormProps) => {
  const {
    Form,
    methods: { handleSubmit },
  } = useConsoleForm({
    schema: checkConstraintFormSchema,
    options: { defaultValues },
  });

  return (
    <Dialog
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
      <div onSubmit={(e) => e.stopPropagation()}>
        <Form onSubmit={onSubmit}>
          <InputField
            name="name"
            label="Constraint name"
            fieldProps={{ placeholder: 'positive_total' }}
          />
          <CodeEditorField
            name="check"
            label="Check expression"
            tooltip="Boolean expression that must be satisfied for all rows in the table. e.g. min_price >= 0 AND max_price >= min_price"
            learnMoreLink="https://www.postgresql.org/docs/current/ddl-constraints.html#DDL-CONSTRAINTS-CHECK-CONSTRAINTS"
            placeholder="total > 0"
            editorProps={{
              mode: 'sql',
              height: '200px',
              width: '100%',
            }}
          />
        </Form>
      </div>
    </Dialog>
  );
};

export default CheckConstraintForm;
