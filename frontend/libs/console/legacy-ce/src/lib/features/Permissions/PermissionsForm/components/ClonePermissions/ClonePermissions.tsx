import React from 'react';
import { useFieldArray } from 'react-hook-form';
import { Button, IconTooltip, Text, useConsoleForm } from '@hasura/shared/ui';
import {
  DataQueryType,
  Metadata,
  Source,
  Table,
  TablePermission,
} from '@hasura/shared/types';
import { Flex, Strong } from '@radix-ui/themes';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import { getDatabaseMethods } from '@hasura/metadata/data-source';
import {
  ClonePermissionItem,
  clonePermissionsSchema,
  useClonePermissions,
} from '@hasura/metadata/api';
import ClonePermissionsRow from './ClonePermissionsRow';
import z from 'zod';

const formKey = 'clonePermissions';

export interface ClonePermissionsSectionProps {
  source: Source;
  table: Table;
  queryType: DataQueryType;
  metadata: Metadata['metadata'];
  onClose: () => void;
  permission: TablePermission;
}

const schema = z.object({
  [formKey]: clonePermissionsSchema,
});

type Schema = z.infer<typeof schema>;

export const ClonePermissionsSection: React.FC<
  ClonePermissionsSectionProps
> = ({ source, onClose, metadata, table, permission, queryType }) => {
  const { clonePermissions, isPending } = useClonePermissions();
  const { methods, Form } = useConsoleForm({
    schema,
    options: {
      defaultValues: {
        clonePermissions: [],
      },
    },
  });

  const { fields, append, remove } = useFieldArray({
    control: methods.control,
    name: 'clonePermissions',
  });

  const watched: ClonePermissionItem[] = methods.watch('clonePermissions');

  const controlledFields = fields.map((field, index) => {
    return {
      ...field,
      ...watched[index],
    };
  });

  React.useEffect(() => {
    const finalRow = controlledFields[controlledFields.length - 1];

    const finalRowIsNotEmpty =
      watched?.length == 0 ||
      (Boolean(finalRow?.table) &&
        finalRow?.queryType !== '' &&
        finalRow?.roleName !== '');

    if (finalRowIsNotEmpty) {
      append({
        table: null,
        queryType: '',
        roleName: '',
      } as ClonePermissionItem);
    }
  }, [controlledFields, append]);

  const roles = MetadataSelectors.getRoles(metadata);
  const dbMethods = getDatabaseMethods(source.kind);

  const queryTypes = dbMethods.config.getSupportedQueryTypes(table);
  const tables = source.tables.map((t) => t.table).filter(Boolean);

  const handleSubmit = (values: Schema) => {
    clonePermissions(
      {
        dataSourceName: source.name,
        from: permission,
        queryType,
        table,
        to: values[formKey],
      },
      {
        onSuccess: () => {
          onClose();
        },
      },
    );
  };

  return (
    <div className="p-4">
      <div className="mb-4">
        <Text data-testid="form-title">
          <Strong>Role:</Strong> {permission.role} <Strong>Action:</Strong>{' '}
          {queryType}
        </Text>
      </div>
      <Form onSubmit={handleSubmit}>
        <Flex className="mb-2" align="center" gap="2">
          <Text as="p">Apply permissions for:</Text>
          <IconTooltip message="Apply same permissions to other tables/actions/roles" />
        </Flex>
        <Flex gap="4" direction="column">
          {controlledFields.map((field, index) => {
            return (
              <ClonePermissionsRow
                key={field.id}
                id={index}
                permission={field}
                tables={tables}
                queryTypes={queryTypes}
                roleNames={roles}
                remove={() => remove(index)}
                disabled={isPending}
              />
            );
          })}
        </Flex>
        <Text>
          <Strong>Note:</Strong> While applying permissions for other tables,
          the column permissions and presets will be ignored
        </Text>
        <Flex align="center" className="mt-4">
          <Button mode="primary" type="submit" loading={isPending}>
            Clone Permissions
          </Button>
        </Flex>
      </Form>
    </div>
  );
};

export default ClonePermissionsSection;
