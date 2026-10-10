import { z } from 'zod';
import {
  CheckboxesField,
  CodeEditorField,
  DelayedDialog,
  DialogFooter,
  ErrorCard,
  getDialogPortalTarget,
  InputField,
  RadioGroupField,
  REACT_SELECT_FILTER_PROPS,
  ReactSelectField,
  useConsoleForm,
} from '@hasura/shared/ui';
import type { TriggerFunction } from '@hasura/metadata/data-source';

const TRIGGER_EVENTS = ['INSERT', 'UPDATE', 'DELETE', 'TRUNCATE'] as const;

const DEFAULT_FUNCTION_BODY = `BEGIN
  -- NEW holds the new row (INSERT/UPDATE), OLD the old one (UPDATE/DELETE).
  RETURN NEW;
END;`;

const schema = z
  .object({
    name: z.string().trim().min(1, { message: 'Trigger name is required' }),
    timing: z.enum(['BEFORE', 'AFTER']),
    events: z
      .array(z.enum(TRIGGER_EVENTS))
      .min(1, { message: 'Select at least one event' }),
    forEach: z.enum(['ROW', 'STATEMENT']),
    condition: z.string(),
    functionMode: z.enum(['existing', 'new']),
    existingFunction: z
      .object({ schema: z.string(), name: z.string() })
      .nullish(),
    newFunctionSchema: z.string().trim(),
    newFunctionName: z.string().trim(),
    newFunctionBody: z.string(),
  })
  .superRefine((values, ctx) => {
    if (values.events.includes('TRUNCATE') && values.forEach === 'ROW') {
      ctx.addIssue({
        code: 'custom',
        path: ['forEach'],
        message: 'TRUNCATE triggers must be FOR EACH STATEMENT',
      });
    }
    if (values.functionMode === 'existing' && !values.existingFunction) {
      ctx.addIssue({
        code: 'custom',
        path: ['existingFunction'],
        message: 'Select a trigger function',
      });
    }
    if (values.functionMode === 'new') {
      if (!values.newFunctionSchema) {
        ctx.addIssue({
          code: 'custom',
          path: ['newFunctionSchema'],
          message: 'Function schema is required',
        });
      }
      if (!values.newFunctionName) {
        ctx.addIssue({
          code: 'custom',
          path: ['newFunctionName'],
          message: 'Function name is required',
        });
      }
      if (!values.newFunctionBody.trim()) {
        ctx.addIssue({
          code: 'custom',
          path: ['newFunctionBody'],
          message: 'Function body is required',
        });
      }
    }
  });

export type TriggerFormValues = z.infer<typeof schema>;

export const getEmptyTriggerFormValues = (
  tableSchema: string,
): TriggerFormValues => ({
  name: '',
  timing: 'BEFORE',
  events: ['INSERT'],
  forEach: 'ROW',
  condition: '',
  functionMode: 'new',
  existingFunction: null,
  newFunctionSchema: tableSchema,
  newFunctionName: '',
  newFunctionBody: DEFAULT_FUNCTION_BODY,
});

export type TriggerFormProps = {
  defaultValues: TriggerFormValues;
  functions: TriggerFunction[];
  isPending?: boolean;
  error?: unknown;
  onSubmit: (values: TriggerFormValues) => void;
  onClose: () => void;
};

/**
 * Postgres `CREATE TRIGGER` dialog. The trigger runs either an existing
 * function returning `trigger`, or a new plpgsql function written here.
 */
export const TriggerForm = ({
  defaultValues,
  functions,
  isPending = false,
  error,
  onSubmit,
  onClose,
}: TriggerFormProps) => {
  const {
    Form,
    methods: { handleSubmit, watch },
  } = useConsoleForm({ schema, options: { defaultValues } });

  const functionMode = watch('functionMode');

  const functionOptions = functions.map((fn) => ({
    value: fn,
    label: `${fn.schema}.${fn.name}`,
  }));

  return (
    <DelayedDialog
      size="lg"
      title="Add Trigger"
      onClose={onClose}
      footer={
        <DialogFooter
          onSubmit={() => {
            handleSubmit(onSubmit)();
          }}
          isLoading={isPending}
          onClose={onClose}
          callToDeny="Cancel"
          callToAction="Add Trigger"
        />
      }
    >
      {/* The dialog is portalled, but React events still bubble through the
          component tree: keep this form's submit from reaching a parent form. */}
      {() => (
        <div onSubmit={(e) => e.stopPropagation()}>
          <Form onSubmit={onSubmit}>
            <InputField
              name="name"
              label="Trigger name"
              fieldProps={{ placeholder: 'my_table_trigger' }}
            />
            <RadioGroupField
              name="timing"
              label="Fire"
              orientation="horizontal"
              options={[
                { value: 'BEFORE', label: 'BEFORE' },
                { value: 'AFTER', label: 'AFTER' },
              ]}
            />
            <CheckboxesField
              name="events"
              label="Events"
              orientation="horizontal"
              options={TRIGGER_EVENTS.map((e) => ({ value: e, label: e }))}
            />
            <RadioGroupField
              name="forEach"
              label="For each"
              orientation="horizontal"
              options={[
                { value: 'ROW', label: 'ROW' },
                { value: 'STATEMENT', label: 'STATEMENT' },
              ]}
            />
            <InputField
              name="condition"
              label="Condition (optional)"
              tooltip="A WHEN condition, e.g. OLD.* IS DISTINCT FROM NEW.*"
              fieldProps={{ placeholder: 'OLD.* IS DISTINCT FROM NEW.*' }}
            />
            <RadioGroupField
              name="functionMode"
              label="Trigger function"
              orientation="horizontal"
              options={[
                { value: 'new', label: 'Create a new function' },
                { value: 'existing', label: 'Use an existing function' },
              ]}
            />
            {functionMode === 'existing' ? (
              <ReactSelectField
                name="existingFunction"
                label="Function"
                placeholder={
                  functions.length
                    ? 'Select a function'
                    : 'No functions returning trigger found'
                }
                options={functionOptions}
                selectProps={{
                  ...REACT_SELECT_FILTER_PROPS,
                  menuPortalTarget: getDialogPortalTarget(),
                }}
              />
            ) : (
              <>
                <div className="grid grid-cols-2 gap-4">
                  <InputField
                    name="newFunctionSchema"
                    label="Function schema"
                  />
                  <InputField
                    name="newFunctionName"
                    label="Function name"
                    fieldProps={{ placeholder: 'my_trigger_fn' }}
                  />
                </div>
                <CodeEditorField
                  name="newFunctionBody"
                  label="Function body (plpgsql)"
                  editorProps={{ mode: 'sql', minLines: 8, maxLines: 30 }}
                />
              </>
            )}
          </Form>
          {error ? (
            <ErrorCard headline="Creating trigger failed" error={error} />
          ) : null}
        </div>
      )}
    </DelayedDialog>
  );
};

export default TriggerForm;
