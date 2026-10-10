import {
  DialogFooter,
  InputField,
  ReactSelectField,
  RawSqlButton,
  SqlCodeBlock,
  Text,
  useConsoleForm,
  getDialogPortalTarget,
  DelayedDialog,
  ErrorCard,
  hasuraToast,
  showErrorNotification,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';
import { TrackableComputedFunction } from '@hasura/metadata/data-source';
import {
  useAddComputedField,
  useDropComputedField,
} from '@hasura/metadata/api';
import {
  areFunctionsEqual,
  functionDisplayName,
} from '@hasura/metadata/helpers';
import {
  MetadataTable,
  PostgresComputedField,
  Source,
  TableFunction,
} from '@hasura/shared/types';
import { schema, Schema } from './schema';

type ComputedFieldDialogProps = {
  source: Source;
  table: MetadataTable;
  functions: TrackableComputedFunction[];
  computedField?: PostgresComputedField;
  onClose: () => void;
};

export const ComputedFieldDialog = ({
  source,
  table,
  functions,
  computedField,
  onClose,
}: ComputedFieldDialogProps) => {
  const isEditMode = Boolean(computedField);

  const { Form, methods } = useConsoleForm({
    schema,
    options: {
      defaultValues: {
        name: computedField?.name ?? '',
        function: computedField ? computedField.definition.function : undefined,
        table_argument: computedField?.definition.table_argument ?? '',
        session_argument: computedField?.definition.session_argument ?? '',
        comment: computedField?.comment ?? '',
      },
    },
  });

  const { watch } = methods;
  const selectedFunction = watch('function') as TableFunction | undefined;

  const {
    mutateAsync: addComputedField,
    isPending: isAdding,
    error: addError,
  } = useAddComputedField();
  const { mutateAsync: dropComputedField, isPending: isDropping } =
    useDropComputedField();

  const isPending = isAdding || isDropping;
  const functionDef = selectedFunction
    ? functions.find((fn) => areFunctionsEqual(fn.function, selectedFunction))
    : undefined;

  // Editing is drop + add, so a failed add would lose the original computed
  // field: put it back as it was.
  const restoreOriginal = async (original: PostgresComputedField) => {
    try {
      await addComputedField({ source, table: table.table, ...original });
    } catch (error) {
      showErrorNotification({
        title: 'Restoring computed field failed',
        message: `"${original.name}" was removed and could not be re-added.`,
        error,
      });
    }
  };

  const onSubmit = async (data: Schema) => {
    let droppedOriginal: PostgresComputedField | undefined;
    try {
      if (isEditMode && computedField) {
        const didDrop = await dropComputedField({
          source,
          table: table.table,
          name: computedField.name,
        })
          .then(() => true)
          .catch((err) => {
            showErrorNotification({
              title: 'Modifying computed field failed',
              error: err,
            });

            return false;
          });

        if (!didDrop) {
          return;
        }
        droppedOriginal = computedField;
      }

      await addComputedField({
        source,
        table: table.table,
        name: data.name,
        definition: {
          function: data.function as TableFunction,
          table_argument: data.table_argument || undefined,
          session_argument: data.session_argument || undefined,
        },
        comment: data.comment || undefined,
      });

      hasuraToast({
        title: 'Success!',
        message: 'Computed field added successfully',
        type: 'success',
      });

      onClose();
    } catch (error) {
      showErrorNotification({
        title: 'Modifying computed field failed',
        error,
      });

      // The add error is shown in the dialog via `addError`.
      if (droppedOriginal) await restoreOriginal(droppedOriginal);
    }
  };

  return (
    <DelayedDialog
      size="md"
      title={isEditMode ? `Edit ${computedField?.name}` : 'Add Computed Field'}
      onClose={onClose}
      footer={
        <DialogFooter
          onSubmit={() => {
            methods.handleSubmit(onSubmit)();
          }}
          isLoading={isPending}
          onClose={onClose}
          callToDeny="Cancel"
          callToAction="Save"
        />
      }
    >
      {() => (
        <div onSubmit={(e) => e.stopPropagation()}>
          <Form onSubmit={() => {}}>
            <InputField
              label="Computed Field Name"
              name="name"
              fieldProps={{
                placeholder: 'Enter computed field name',
                disabled: isEditMode,
              }}
            />

            <ReactSelectField
              label="Function"
              name="function"
              placeholder="Select a function"
              options={functions.map((fn) => ({
                value: fn.function,
                label: functionDisplayName({ qualifiedFunction: fn.function }),
              }))}
              selectProps={{
                isSearchable: true,
                menuPortalTarget: getDialogPortalTarget(),
                filterOption: (option, inputValue) => {
                  return (
                    !inputValue ||
                    option.label
                      .toLowerCase()
                      .includes(inputValue.toLowerCase())
                  );
                },
              }}
            />

            {functionDef && (
              <div className="mb-4">
                <Flex align="center" gap="2" className="mb-2">
                  <Text weight="medium">Function Definition</Text>
                  {functionDef.definition && (
                    <RawSqlButton
                      sql={functionDef.definition}
                      source={source.name}
                    >
                      Modify
                    </RawSqlButton>
                  )}
                </Flex>
                <SqlCodeBlock
                  language={source.kind}
                  text={functionDef.definition ?? '-- Function not found'}
                />
              </div>
            )}

            <InputField
              label="Table Row Argument"
              name="table_argument"
              fieldProps={{ placeholder: 'default: first argument' }}
              tooltip="The argument of the function to which the table row is passed. By default, the first argument of the function is assumed to be the table row argument"
            />

            <InputField
              label="Session Argument"
              name="session_argument"
              fieldProps={{ placeholder: 'hasura_session' }}
              tooltip="The function argument into which Hasura session variables will be passed"
              learnMoreLink="https://hasura.io/docs/latest/graphql/core/schema/computed-fields.html#accessing-hasura-session-variables-in-computed-fields"
            />

            <InputField
              label="Comment"
              name="comment"
              fieldProps={{ placeholder: 'Add a comment' }}
            />
            {addError ? (
              <ErrorCard
                headline="Adding computed field error"
                error={addError}
              />
            ) : null}
          </Form>
        </div>
      )}
    </DelayedDialog>
  );
};
