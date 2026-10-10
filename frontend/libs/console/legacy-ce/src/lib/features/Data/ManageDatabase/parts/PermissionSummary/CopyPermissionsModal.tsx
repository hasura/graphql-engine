import { useBulkCopyPermissionsByRole } from '@hasura/metadata/api';
import {
  DATA_QUERY_TYPES,
  DataQueryType,
  QualifiedDataSource,
  Table,
} from '@hasura/shared/types';
import {
  Button,
  Dialog,
  getDialogPortalTarget,
  IconTooltip,
  InputField,
  ReactSelectField,
  SelectField,
  SwitchField,
  Text,
  useConsoleForm,
} from '@hasura/shared/ui';
import { getTableDisplayName } from '@hasura/shared/utils';
import { Flex } from '@radix-ui/themes';
import { useEffect, useState } from 'react';
import { FaCopy } from 'react-icons/fa6';
import { createFilter } from 'react-select';
import { z } from 'zod';

const ALL_TABLES_VALUE = '__all_tables__';

const queryTypeOptions: { label: string; value: DataQueryType }[] = [
  { label: 'select', value: 'select' },
  { label: 'insert', value: 'insert' },
  { label: 'update', value: 'update' },
  { label: 'delete', value: 'delete' },
];

const schema = z
  .object({
    fromTable: z.string().optional(),
    toTables: z.string().optional(),
    allTables: z.boolean(),
    fromRole: z.string().min(1, 'Role is required'),
    queryType: z.enum(DATA_QUERY_TYPES),
    createNewRole: z.boolean(),
    newRole: z.string().optional(),
    toRole: z.string().optional(),
  })
  .refine(
    (values) => (values.createNewRole ? Boolean(values.newRole?.trim()) : true),
    { message: 'New role name is required', path: ['newRole'] },
  )
  .refine((values) => (values.allTables ? true : values.fromTable), {
    message: 'From table is required',
    path: ['fromTable'],
  })
  .refine((values) => (values.allTables ? true : values.toTables), {
    message: 'To table is required',
    path: ['toTables'],
  })
  .refine((values) => (!values.createNewRole ? Boolean(values.toRole) : true), {
    message: 'Role is required',
    path: ['toRole'],
  });

type FormValues = z.infer<typeof schema>;

type Props = {
  source: QualifiedDataSource;
  roles: string[];
  tables: Table[];
  onClose: () => void;
};

const CopyPermissionsModal = ({ onClose, source, roles, tables }: Props) => {
  const { bulkCopyPermissionsByRole, isPending } =
    useBulkCopyPermissionsByRole();
  const [dialogLoaded, setDialogLoaded] = useState(false);

  useEffect(() => {
    setTimeout(() => {
      // Hack: the dialog portal target needs to be rendered before the ReactSelect component.
      setDialogLoaded(true);
    }, 200);
  }, []);

  const { methods, Form } = useConsoleForm({
    schema,
    options: {
      defaultValues: {
        toTables: ALL_TABLES_VALUE,
        fromRole: '',
        queryType: 'select',
        createNewRole: false,
        allTables: false,
        newRole: '',
        toRole: '',
      },
    },
  });

  const [createNewRole, allTables] = methods.watch([
    'createNewRole',
    'allTables',
  ]);

  const tableOptions = tables.map((table) => ({
    label: getTableDisplayName(table),
    value: table,
  }));

  const onSubmit = (values: FormValues) => {
    const fromTable =
      values.fromTable === ALL_TABLES_VALUE
        ? undefined
        : tableOptions.find((opt) => opt.label === values.fromTable)?.value;
    const toTable =
      values.toTables === ALL_TABLES_VALUE
        ? undefined
        : tableOptions.find((opt) => opt.label === values.toTables)?.value;
    if (!values.allTables && (!fromTable || !toTable)) {
      return;
    }

    bulkCopyPermissionsByRole(
      {
        fromRole: values.fromRole,
        actions: [values.queryType],
        source: source.name,
        toRoles: [
          values.createNewRole ? (values.newRole ?? '') : (values.toRole ?? ''),
        ],
        ...(values.allTables
          ? {
              allTables: true,
            }
          : {
              allTables: false,
              fromTable: fromTable!,
              toTable: toTable!,
            }),
      },
      {
        onSuccess: () => {
          onClose();
        },
      },
    );
  };

  const tableLabelOptions = tableOptions.map((table) => ({
    label: table.label,
    value: table.label,
  }));

  return (
    <Dialog
      title="Copy permissions"
      onClose={onClose}
      footer={{
        disabled: isPending,
        isLoading: isPending,
        callToAction: 'Copy',
        onSubmit: methods.handleSubmit(onSubmit),
      }}
    >
      <Form onSubmit={onSubmit}>
        {dialogLoaded ? (
          <CopyPermissionsForm
            isPending={isPending}
            roles={roles}
            tableLabelOptions={tableLabelOptions}
            createNewRole={createNewRole}
            allTables={allTables}
          />
        ) : null}
      </Form>
    </Dialog>
  );
};

const CopyPermissionsForm = ({
  roles,
  isPending,
  tableLabelOptions,
  createNewRole,
  allTables,
}: {
  isPending: boolean;
  createNewRole: boolean;
  allTables: boolean;
  roles: string[];
  tableLabelOptions: {
    label: string;
    value: string;
  }[];
}) => {
  const roleOptions = roles.map((role) => ({ label: role, value: role }));

  const filterOption = createFilter({
    ignoreCase: true,
    matchFrom: 'any',
  });

  const commonSelectProps = {
    isSearchable: true,
    menuPortalTarget: getDialogPortalTarget(),
    filterOption,
  };

  return (
    <Flex direction="column" gap="4">
      <SwitchField name="allTables" noErrorPlaceholder>
        <Flex align="center" gap="2">
          <Text>Clone all permissions</Text>
          <IconTooltip message="Copy same permissions of all tables from role A to role B" />
        </Flex>
      </SwitchField>
      <Text as="p" weight="bold">
        From:
      </Text>
      <ReactSelectField
        name="fromRole"
        label="Role"
        placeholder="Select role"
        noErrorPlaceholder
        options={roleOptions}
        selectProps={commonSelectProps}
        disabled={isPending}
      />
      <SelectField
        name="queryType"
        label="Permission Type"
        full
        noErrorPlaceholder
        options={queryTypeOptions}
        disabled={isPending}
      />
      {!allTables && (
        <ReactSelectField
          name="fromTable"
          label="Table"
          placeholder="Select table"
          noErrorPlaceholder
          options={tableLabelOptions}
          selectProps={commonSelectProps}
          disabled={isPending}
        />
      )}
      <Text as="p" weight="bold">
        To:
      </Text>
      <SwitchField noErrorPlaceholder name="createNewRole" disabled={isPending}>
        Create new role
      </SwitchField>
      {createNewRole ? (
        <InputField
          name="newRole"
          label="Role"
          noErrorPlaceholder
          fieldProps={{
            placeholder: 'New role name',
            disabled: isPending,
          }}
        />
      ) : (
        <ReactSelectField
          name="toRole"
          label="Role"
          placeholder="Select role"
          noErrorPlaceholder
          options={roleOptions}
          selectProps={commonSelectProps}
          disabled={isPending}
        />
      )}
      {!allTables && (
        <ReactSelectField
          name="toTables"
          label="Table"
          placeholder="Select table"
          noErrorPlaceholder
          options={[
            { label: 'All tables', value: ALL_TABLES_VALUE },
            ...tableLabelOptions,
          ]}
          selectProps={{
            ...commonSelectProps,
            menuPlacement: 'top',
          }}
          disabled={isPending}
        />
      )}
    </Flex>
  );
};

export const CopyPermissionsButton = (props: Omit<Props, 'onClose'>) => {
  const [open, setOpen] = useState(false);

  return (
    <>
      <Button
        mode="default"
        size="sm"
        onClick={() => setOpen(true)}
        leftIcon={FaCopy}
      >
        Copy Permissions
      </Button>
      {open ? (
        <CopyPermissionsModal {...props} onClose={() => setOpen(false)} />
      ) : null}
    </>
  );
};
