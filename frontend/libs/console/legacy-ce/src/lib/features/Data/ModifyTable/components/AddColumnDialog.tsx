import { useMemo, useState } from 'react';
import { z } from 'zod';
import { Flex } from '@radix-ui/themes';
import {
  Button,
  CheckboxField,
  DelayedDialog,
  DialogFooter,
  ErrorCard,
  getDialogPortalTarget,
  InputField,
  REACT_SELECT_FILTER_PROPS,
  ReactSelectField,
  Text,
  useConsoleForm,
} from '@hasura/shared/ui';
import {
  DependentSQLGenerator,
  FrequentlyUsedColumn,
  getDatabaseMethods,
  useAddColumn,
  useSupportedDataTypes,
} from '@hasura/metadata/data-source';
import { ModifyTableProps } from '../types';

const schema = z.object({
  name: z.string().trim().min(1, { message: 'Column name is required' }),
  type: z.string().trim().min(1, { message: 'Column type is required' }),
  nullable: z.boolean(),
  unique: z.boolean(),
  default: z.string(),
});

type AddColumnFormValues = z.infer<typeof schema>;

const emptyValues: AddColumnFormValues = {
  name: '',
  type: '',
  nullable: true,
  unique: false,
  default: '',
};

type AddColumnDialogProps = Pick<ModifyTableProps, 'source' | 'table'> & {
  onClose: () => void;
};

/** Adds a column to an existing table (`ALTER TABLE ... ADD COLUMN`). */
export const AddColumnDialog = ({
  source,
  table,
  onClose,
}: AddColumnDialogProps) => {
  const {
    Form,
    methods: { handleSubmit, reset },
  } = useConsoleForm({ schema, options: { defaultValues: emptyValues } });

  // Extra SQL from a picked preset (e.g. the `updated_at` trigger); dropped as
  // soon as the type is changed away from the preset's.
  const [preset, setPreset] = useState<{
    type: string;
    dependentSQLGenerator?: DependentSQLGenerator;
  }>();

  const presets = (
    getDatabaseMethods(source.kind).config?.getFrequentlyUsedColumns?.() ?? []
  ).filter((c) => c.validFor.includes('modify'));

  const applyPreset = (column: FrequentlyUsedColumn) => {
    reset({
      ...emptyValues,
      name: column.name,
      type: column.type,
      default: column.default ?? '',
    });
    setPreset({
      type: column.type,
      dependentSQLGenerator: column.dependentSQLGenerator,
    });
  };

  const { data: dataTypeMap } = useSupportedDataTypes(source.kind);
  const typeOptions = useMemo(() => {
    const allTypes = dataTypeMap ? Object.values(dataTypeMap).flat() : [];
    return Array.from(new Set(allTypes)).map((type) => ({
      value: type,
      label: type,
    }));
  }, [dataTypeMap]);

  const {
    mutate: addColumn,
    isPending,
    error,
  } = useAddColumn({ onSuccess: () => onClose() });

  const onSubmit = (values: AddColumnFormValues) => {
    addColumn({
      source: { name: source.name, kind: source.kind },
      table: table.table,
      column: {
        name: values.name,
        type: values.type,
        nullable: values.nullable,
        unique: values.unique,
        default: values.default.trim()
          ? { value: values.default.trim() }
          : undefined,
        dependentSQLGenerator:
          preset?.type === values.type
            ? preset.dependentSQLGenerator
            : undefined,
      },
    });
  };

  return (
    <DelayedDialog
      size="md"
      title="Add Column"
      onClose={onClose}
      footer={
        <DialogFooter
          onSubmit={() => {
            handleSubmit(onSubmit)();
          }}
          isLoading={isPending}
          onClose={onClose}
          callToDeny="Cancel"
          callToAction="Add Column"
        />
      }
    >
      {/* The dialog is portalled, but React events still bubble through the
          component tree: keep this form's submit from reaching a parent form. */}
      {() => (
        <div onSubmit={(e) => e.stopPropagation()}>
          {presets.length > 0 && (
            <Flex align="center" gap="2" wrap="wrap" className="mb-4">
              <Text size="2" color="gray">
                Frequently used:
              </Text>
              {presets.map((column) => (
                <Button
                  key={`${column.name}-${column.type}`}
                  type="button"
                  size="1"
                  mode="default"
                  title={column.defaultText ?? column.typeText}
                  onClick={() => applyPreset(column)}
                >
                  {column.name}
                </Button>
              ))}
            </Flex>
          )}
          <Form onSubmit={onSubmit}>
            <InputField
              name="name"
              label="Name"
              fieldProps={{ placeholder: 'column_name' }}
            />
            <ReactSelectField
              name="type"
              label="Type"
              placeholder="Select a type"
              options={typeOptions}
              selectProps={{
                ...REACT_SELECT_FILTER_PROPS,
                menuPortalTarget: getDialogPortalTarget(),
              }}
            />
            <InputField
              name="default"
              label="Default"
              tooltip="A value or SQL function, e.g. now(). Text values are quoted for you."
              fieldProps={{ placeholder: '(no default)' }}
            />
            <CheckboxField name="nullable">Nullable</CheckboxField>
            <CheckboxField name="unique">Unique</CheckboxField>
          </Form>
          {error ? (
            <ErrorCard headline="Adding column failed" error={error} />
          ) : null}
        </div>
      )}
    </DelayedDialog>
  );
};

export default AddColumnDialog;
