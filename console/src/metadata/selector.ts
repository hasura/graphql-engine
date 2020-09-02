import { createSelector } from 'reselect';
import { ReduxState } from '../types';
import { TableEntry } from './types';
import { filterInconsistentMetadataObjects } from '../components/Services/Settings/utils';

const getInconsistentObjects = (state: ReduxState) => {
  return state.metadata.inconsistentObjects;
};

const getTables = (state: ReduxState) => {
  return state.metadata.metadataObject?.tables;
};

const getActions = (state: ReduxState) => {
  return state.metadata.metadataObject?.actions;
};

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
export const rolesSelector = createSelector(
  [getTables, getActions],
  (tables, actions) => {
    const roleNames: string[] = [];
    tables?.forEach(table =>
      permKeys.forEach(key =>
        table[key]?.forEach(({ role }: { role: string }) =>
          roleNames.push(role)
        )
      )
    );
    actions?.forEach(action =>
      action.permissions?.forEach(p => roleNames.push(p.role))
    );
    return Array.from(new Set(roleNames));
  }
);

const getRemoteSchemas = (state: ReduxState) => {
  return state.metadata.metadataObject?.remote_schemas || [];
};

export const getRemoteSchemasSelector = createSelector(
  [getRemoteSchemas, getInconsistentObjects],
  (schemas, inconsistentObjects) => {
    return filterInconsistentMetadataObjects(
      schemas,
      inconsistentObjects,
      'remote_schemas'
    );
  }
);

export const remoteSchemasNamesSelector = createSelector(
  getRemoteSchemas,
  schemas => schemas?.map(schema => schema.name)
);
