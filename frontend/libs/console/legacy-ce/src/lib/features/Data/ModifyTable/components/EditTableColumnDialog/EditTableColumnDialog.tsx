import { useMemo } from 'react';
import {
  CheckboxField,
  DelayedDialog,
  DialogFooter,
  ErrorCard,
  getDialogPortalTarget,
  InputField,
  REACT_SELECT_FILTER_PROPS,
  ReactSelectField,
  SanitizeTips,
  useConsoleForm,
} from '@hasura/shared/ui';
import {
  columnDataType,
  getDatabaseMethods,
  useAlterColumn,
  useSupportedDataTypes,
} from '@hasura/metadata/data-source';
import { sanitizeGraphQLFieldNames } from '@hasura/shared/utils';
import { useUpdateTableConfiguration } from '../../../ModifyTable/hooks';
import { ModifyTableColumn, ModifyTableProps } from '../../types';
import { ColumnFormValues, columnFormSchema, schema } from './schema';
import { Flex } from '@radix-ui/themes';

interface EditTableColumnDialogProps extends ModifyTableProps {
  onClose: () => void;
  column: ModifyTableColumn;
  /** Name of the column's single-column unique constraint, if it has one. */
  uniqueConstraintName?: string;
}

export const EditTableColumnDialog = ({
  onClose,
  column,
  uniqueConstraintName,
  table: metadataTable,
  source,
  isView,
}: EditTableColumnDialogProps) => {
  // Altering the column itself needs the driver method, and the column's
  // default introspected so an untouched default isn't mistaken for a removal.
  const canAlterColumn =
    !isView &&
    Boolean(getDatabaseMethods(source.kind).modify?.alterColumn) &&
    column.defaultValue !== undefined;

  const currentType = column.sqlType ?? columnDataType(column.dataType);
  const currentColumn = {
    name: column.name,
    type: currentType,
    nullable: Boolean(column.nullable),
    default: column.defaultValue ?? '',
    unique: Boolean(uniqueConstraintName),
  };

  const {
    Form,
    methods: { handleSubmit },
  } = useConsoleForm({
    schema: columnFormSchema,
    options: {
      defaultValues: {
        ...currentColumn,
        custom_name: column.config?.custom_name ?? '',
        comment: column.config?.comment ?? '',
      },
    },
  });

  const { data: dataTypeMap } = useSupportedDataTypes(source.kind);
  const typeOptions = useMemo(() => {
    const allTypes = dataTypeMap ? Object.values(dataTypeMap).flat() : [];
    // Keep the column's current type selectable even if it isn't a preset
    // (e.g. `character varying(255)` or an array type).
    return Array.from(new Set([currentType, ...allTypes])).map((type) => ({
      value: type,
      label: type,
    }));
  }, [dataTypeMap, currentType]);

  const { isPending: isSavingConfig, updateTableConfiguration } =
    useUpdateTableConfiguration(source.name, metadataTable.table);

  const {
    mutateAsync: alterColumn,
    isPending: isAlteringColumn,
    error: alterColumnError,
    reset: resetAlterColumn,
  } = useAlterColumn();

  const onSubmit = async (values: ColumnFormValues) => {
    resetAlterColumn();

    // Metadata first, keyed by the current column name: when the column is
    // renamed below, the server moves its column config to the new name.
    const config = schema.parse({
      custom_name: values.custom_name.trim(),
      comment: values.comment.trim(),
    });
    const configChanged =
      (config.custom_name ?? '') !== (column.config?.custom_name ?? '') ||
      (config.comment ?? '') !== (column.config?.comment ?? '');

    try {
      if (configChanged) {
        await updateTableConfiguration({
          column_config: {
            ...metadataTable.configuration?.column_config,
            [column.name]: config,
          },
          custom_column_names: {
            ...metadataTable.configuration?.custom_column_names,
            [column.name]: config.custom_name,
          },
        });
      }

      if (canAlterColumn) {
        await alterColumn({
          source: { name: source.name, kind: source.kind },
          table: metadataTable.table,
          previous: { ...currentColumn, uniqueConstraintName },
          next: {
            name: values.name,
            type: values.type,
            nullable: values.nullable,
            default: values.default,
            unique: values.unique,
          },
        });
      }

      onClose();
    } catch {
      // Configuration errors are notified by the hook; column errors are shown
      // below. Keep the dialog open either way.
    }
  };

  return (
    <DelayedDialog
      size="md"
      title={`Edit column ${column.name}`}
      onClose={onClose}
      footer={
        <DialogFooter
          onSubmit={() => {
            handleSubmit(onSubmit)();
          }}
          isLoading={isSavingConfig || isAlteringColumn}
          onClose={onClose}
          callToDeny="Cancel"
          callToAction="Save"
        />
      }
    >
      {/* The dialog is portalled, but React events still bubble through the
          component tree: keep this form's submit from reaching a parent form. */}
      {() => (
        <div onSubmit={(e) => e.stopPropagation()}>
          <Form onSubmit={onSubmit}>
            {canAlterColumn && (
              <>
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
                  tooltip="A SQL expression, e.g. now(), 0 or 'text'. Leave empty for no default."
                  fieldProps={{ placeholder: '(no default)' }}
                />
                <Flex align="center" gap="4">
                  <CheckboxField
                    name="nullable"
                    disabled={column.isPrimaryKey}
                    tooltip={
                      column.isPrimaryKey
                        ? 'Primary key columns cannot be nullable'
                        : undefined
                    }
                  >
                    Nullable
                  </CheckboxField>
                  <CheckboxField
                    name="unique"
                    disabled={column.isPrimaryKey}
                    tooltip={
                      column.isPrimaryKey
                        ? 'Primary key columns are already unique'
                        : undefined
                    }
                  >
                    Unique
                  </CheckboxField>
                </Flex>
              </>
            )}
            <SanitizeTips />
            <InputField
              label="GraphQL Field Name"
              name="custom_name"
              fieldProps={{ placeholder: `${column.name} (default)` }}
              tooltip="Expose the column with a different name in the GraphQL API"
              inputTransform={(val) => sanitizeGraphQLFieldNames(val)}
            />
            <InputField
              tooltip="Add a comment for your table column"
              label="Comment"
              name="comment"
              fieldProps={{ placeholder: 'Add a comment' }}
            />
          </Form>
          {alterColumnError ? (
            <ErrorCard
              headline="Modifying column failed"
              error={alterColumnError}
            />
          ) : null}
        </div>
      )}
    </DelayedDialog>
  );
};
