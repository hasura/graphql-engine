import { TableEntry } from '../../../metadata/types';
import { MetadataSelector } from './metadataSelectors';
import { useMetadata } from './useMetadata';

type PermKeys = Pick<
  TableEntry,
  | 'update_permissions'
  | 'select_permissions'
  | 'delete_permissions'
  | 'insert_permissions'
>;
const permKeys: Array<keyof PermKeys> = [
  'insert_permissions',
  'update_permissions',
  'select_permissions',
  'delete_permissions',
];

export function useRoles() {
  const { data: actions, ...rest } = useMetadata(d => d.metadata.actions);
  const { data: tableEntries } = useMetadata(
    MetadataSelector.getTablesFromAllSources
  );
  const { data: remoteSchemas } = useMetadata(d => d.metadata.remote_schemas);
  const { data: allowlists } = useMetadata(d => d.metadata.allowlist);
  const { data: securitySettings } = useMetadata(
    MetadataSelector.getSecuritySettings
  );
  const roleNames: string[] = [];
  tableEntries?.forEach(table =>
    permKeys.forEach(key =>
      table[key]?.forEach(({ role }: { role: string }) => roleNames.push(role))
    )
  );
  actions?.forEach(action =>
    action.permissions?.forEach(p => roleNames.push(p.role))
  );
  remoteSchemas?.forEach(remoteSchema => {
    remoteSchema?.permissions?.forEach(p => roleNames.push(p.role));
  });
  allowlists?.forEach(allowlist => {
    if (allowlist?.scope?.global === false) {
      allowlist?.scope?.roles?.forEach(role => roleNames.push(role));
    }
  });

  Object.entries(securitySettings?.api_limits ?? {}).forEach(
    ([limit, value]) => {
      if (limit !== 'disabled' && typeof value !== 'boolean') {
        Object.keys(value?.per_role ?? {}).forEach(role =>
          roleNames.push(role)
        );
      }
    }
  );
  securitySettings?.graphql_schema_introspection?.disabled_for_roles.forEach(
    role => roleNames.push(role)
  );
  return { data: Array.from(new Set(roleNames)), ...rest };
}
