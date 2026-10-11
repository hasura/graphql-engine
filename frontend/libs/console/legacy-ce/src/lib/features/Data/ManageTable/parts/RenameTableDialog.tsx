import { z } from 'zod';
import { useNavigate, useParams } from 'react-router';
import {
  DelayedDialog,
  DialogFooter,
  ErrorCard,
  InputField,
  useConsoleForm,
} from '@hasura/shared/ui';
import { Source, Table } from '@hasura/shared/types';
import { areTablesEqual } from '@hasura/metadata/helpers';
import { dataRoutes, gqlPattern } from '@hasura/shared/utils';
import {
  useChangeTableName,
  useTrackableTables,
} from '@hasura/metadata/data-source';

type RenameTableDialogProps = {
  source: Source;
  table: Table;
  /** current name of the table, without the schema */
  currentName: string;
  onClose: () => void;
};

/** Renames a table or view (`ALTER TABLE ... RENAME TO ...`). */
export const RenameTableDialog = ({
  source,
  table,
  currentName,
  onClose,
}: RenameTableDialogProps) => {
  const navigate = useNavigate();
  const { operation } = useParams<{ operation: string }>();

  // the rename SQL differs for tables, views and materialized views
  const { data: tableType, isLoading: isLoadingTableType } = useTrackableTables<
    string | undefined
  >(
    { source },
    {
      select: (tables) =>
        tables.find((t) => areTablesEqual(t.table, table))?.type,
    },
  );

  const schema = z.object({
    name: z
      .string()
      .trim()
      .min(1, { message: 'Name is required' })
      .regex(gqlPattern, {
        message:
          'Name must start with a letter or underscore and contain only letters, digits and underscores',
      })
      .refine((name) => name !== currentName, {
        message: `The name is already ${currentName}`,
      }),
  });

  type RenameTableFormValues = z.infer<typeof schema>;

  const {
    Form,
    methods: { handleSubmit },
  } = useConsoleForm({
    schema,
    options: { defaultValues: { name: currentName } },
  });

  const {
    mutate: changeTableName,
    isPending,
    error,
  } = useChangeTableName({
    onSuccess: (renamed, { newName }) => {
      if (!renamed) return;
      onClose();
      // the old URL points to a table that no longer exists
      navigate(
        dataRoutes.manageTable(
          source.name,
          { ...(table as Record<string, unknown>), name: newName } as Table,
          operation,
        ),
      );
    },
  });

  const onSubmit = (values: RenameTableFormValues) => {
    if (!tableType) return;
    changeTableName({
      source: { name: source.name, kind: source.kind },
      table: { table, type: tableType },
      newName: values.name,
    });
  };

  return (
    <DelayedDialog
      size="sm"
      title="Rename table"
      onClose={onClose}
      footer={
        <DialogFooter
          onSubmit={() => {
            handleSubmit(onSubmit)();
          }}
          isLoading={isPending}
          disabled={isLoadingTableType || !tableType}
          onClose={onClose}
          callToDeny="Cancel"
          callToAction="Rename"
        />
      }
    >
      {() => (
        <div onSubmit={(e) => e.stopPropagation()}>
          <Form onSubmit={onSubmit}>
            <InputField
              name="name"
              label="Name"
              fieldProps={{ placeholder: 'table_name', autoFocus: true }}
            />
          </Form>
          {error ? (
            <ErrorCard headline="Renaming table failed" error={error} />
          ) : null}
        </div>
      )}
    </DelayedDialog>
  );
};
