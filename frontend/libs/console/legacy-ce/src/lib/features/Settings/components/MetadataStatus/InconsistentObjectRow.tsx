import {
  InconsistentObjectFields,
  InconsistentObjectInheritedRole,
  Metadata,
} from '@hasura/shared/types';
import { Button, JsonCodeBlock, Text } from '@hasura/shared/ui';
import { extractTableInfo, getTableDisplayName } from '@hasura/shared/utils';
import { NavigateFunction } from 'react-router';
import { Strong } from '@radix-ui/themes';

export const resolveInconsistentInheritedRole = (
  navigate: NavigateFunction,
  metadata: Metadata['metadata'] | undefined,
  inconsistentInheritedRoleObj: InconsistentObjectInheritedRole,
) => {
  if ('remote_schema' in inconsistentInheritedRoleObj.entity) {
    navigate(
      `/remote-schemas/manage/${inconsistentInheritedRoleObj.entity.remote_schema}/permissions`,
    );
    return;
  }

  const { table, source, permission_type } =
    inconsistentInheritedRoleObj.entity;
  const role = inconsistentInheritedRoleObj.name;
  const tableSource = metadata?.sources.find((s) => s.name === source);
  if (!tableSource) {
    return;
  }
  const tableSchema = tableSource.tables
    .map((t) => extractTableInfo(t.table))
    .find((t) => t?.name === table);
  if (!tableSchema) {
    return;
  }

  navigate(
    `/data/${source}/schema/${tableSchema.schema}/tables/${table}/permissions?role=${role}&permission_type=${permission_type}`,
  );
};

export const getInconsistentObjectDisplay = (
  inconsistentObject: InconsistentObjectFields,
) => {
  let name = '';
  let definition = '';

  switch (inconsistentObject.type) {
    case 'source':
      name = inconsistentObject.definition as string;
      break;
    case 'object_relation':
    case 'array_relation':
      name = inconsistentObject.definition.name;
      definition = `relationship of table "${getTableDisplayName(
        inconsistentObject.definition.table,
      )}"`;
      break;
    case 'remote_relationship':
      name = inconsistentObject.definition.name;
      definition = `relationship between table "${getTableDisplayName(
        inconsistentObject.definition.table,
      )}" and remote schema "${inconsistentObject.definition.remote_schema}"`;
      break;
    case 'select_permission':
    case 'insert_permission':
    case 'update_permission':
    case 'delete_permission':
      name = `${inconsistentObject.definition.role}-permission`;
      definition = `${inconsistentObject.type} on table "${getTableDisplayName(
        inconsistentObject.definition.table,
      )}"`;
      break;
    case 'table':
      name = getTableDisplayName(inconsistentObject.definition);
      definition = name;
      break;
    case 'function':
      name = getTableDisplayName(inconsistentObject.definition as string);
      definition = name;
      break;
    case 'event_trigger':
      name = inconsistentObject.definition.configuration.name;
      definition = `event trigger on table "${getTableDisplayName(
        inconsistentObject.definition.table,
      )}"`;
      break;
    case 'remote_schema': {
      name = inconsistentObject.definition.name;
      let url = `"${
        inconsistentObject.definition.definition.url ||
        inconsistentObject.definition.definition.url_from_env
      }"`;
      if (inconsistentObject.definition.definition.url_from_env) {
        url = `the url from the value of env var ${url}`;
      }
      definition = `remote schema named "${name}" at ${url}`;
      break;
    }
  }

  return { name, definition };
};

export const InconsistentObjectActionCell = ({
  inconsistentObject,
  onResolve,
}: {
  inconsistentObject: InconsistentObjectFields;
  onResolve: (
    inconsistentInheritedRoleObj: InconsistentObjectInheritedRole,
  ) => void;
}) => {
  if (inconsistentObject.type !== 'inherited role permission inconsistency') {
    return null;
  }

  return (
    <Button mode="default" onClick={() => onResolve(inconsistentObject)}>
      Resolve
    </Button>
  );
};

export const InconsistentObjectReasonCell = ({
  inconsistentObject,
}: {
  inconsistentObject: InconsistentObjectFields;
}) => {
  const message = inconsistentObject.message;
  return (
    <Text wrap="pretty">
      <Strong>{inconsistentObject.reason}</Strong>
      <br />
      {typeof message === 'string' ? (
        message
      ) : message ? (
        <JsonCodeBlock value={message} className="mt-2" />
      ) : null}
    </Text>
  );
};
