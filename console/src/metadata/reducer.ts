import { MetadataActions } from './actions';
import {
  HasuraMetadataV3,
  CollectionName,
  InheritedRole,
  InconsistentObject,
} from './types';
import { setAllowedQueries } from './utils';

export type AllowedQueriesCollection = {
  name: string;
  query: string;
  collection: CollectionName;
};

type MetadataState = {
  metadataObject: null | HasuraMetadataV3;
  resourceVersion: number;
  error: null | string | boolean;
  loading: boolean;
  inconsistentObjects: InconsistentObject[];
  inconsistentInheritedRoles: any[];
  ongoingRequest: boolean; // deprecate
  allowedQueries: AllowedQueriesCollection[];
  inheritedRoles: InheritedRole[];
};

const defaultState: MetadataState = {
  metadataObject: null,
  resourceVersion: 1,
  error: null,
  loading: false,
  inconsistentObjects: [],
  inconsistentInheritedRoles: [],
  ongoingRequest: false,
  allowedQueries: [],
  inheritedRoles: [],
};

const renameSourceAttributes = (sources: HasuraMetadataV3['sources']) =>
  sources.map((s: any) => {
    let tables = s.tables;
    if (s.kind === 'bigquery') {
      tables = s.tables.map((t: any) => {
        let object_relationships = [];
        if (t.object_relationships) {
          object_relationships = t.object_relationships.map((objRel: any) => {
            return {
              ...objRel,
              using: {
                ...objRel.using,
                manual_configuration: {
                  ...objRel.using.manual_configuration,
                  remote_table: {
                    schema:
                      objRel.using.manual_configuration.remote_table.dataset,
                    name: objRel.using.manual_configuration.remote_table.name,
                  },
                },
              },
            };
          });
        }

        let array_relationships = [];
        if (t.array_relationships) {
          array_relationships = t.array_relationships.map((objRel: any) => {
            return {
              ...objRel,
              using: {
                ...objRel.using,
                manual_configuration: {
                  ...objRel.using.manual_configuration,
                  remote_table: {
                    schema:
                      objRel.using.manual_configuration.remote_table.dataset,
                    name: objRel.using.manual_configuration.remote_table.name,
                  },
                },
              },
            };
          });
        }

        return {
          object_relationships,
          array_relationships,
          table: {
            name: t.table.name,
            schema: t.table.dataset,
          },
          select_permissions: t.select_permissions,
        };
      });
    }

    return { ...s, tables };
  });

export const metadataReducer = (
  state = defaultState,
  action: MetadataActions
): MetadataState => {
  switch (action.type) {
    case 'Metadata/EXPORT_METADATA_SUCCESS':
      const metadata =
        'metadata' in action.data ? action.data.metadata : action.data;
      return {
        ...state,
        metadataObject: {
          ...metadata,
          sources: renameSourceAttributes(metadata.sources),
        },
        resourceVersion:
          'resource_version' in action.data ? action.data.resource_version : 1,
        allowedQueries: setAllowedQueries(
          metadata?.query_collections,
          metadata?.allowlist
        ),
        inheritedRoles: metadata?.inherited_roles,
        loading: false,
        error: null,
      };
    case 'Metadata/EXPORT_METADATA_REQUEST':
      return {
        ...state,
        loading: true,
        error: null,
      };
    case 'Metadata/EXPORT_METADATA_ERROR':
      return {
        ...state,
        loading: false,
        error: action.data,
      };

    case 'Metadata/LOAD_INCONSISTENT_OBJECTS_SUCCESS':
      return {
        ...state,
        inconsistentObjects: action.data.filter(
          t => t.type !== 'inherited role permission inconsistency'
        ),
        inconsistentInheritedRoles: action.data.filter(
          t => t.type === 'inherited role permission inconsistency'
        ),
        ongoingRequest: false,
      };
    case 'Metadata/LOAD_INCONSISTENT_OBJECTS_REQUEST':
      return {
        ...state,
        ongoingRequest: true,
      };
    case 'Metadata/LOAD_INCONSISTENT_OBJECTS_ERROR':
      return {
        ...state,
        error: true,
        ongoingRequest: false,
      };

    case 'Metadata/DROP_INCONSISTENT_METADATA_SUCCESS':
      return {
        ...state,
        inconsistentObjects: [],
        ongoingRequest: false,
      };
    case 'Metadata/DROP_INCONSISTENT_METADATA_REQUEST':
      return {
        ...state,
        ongoingRequest: true,
      };
    case 'Metadata/DROP_INCONSISTENT_METADATA_ERROR':
      return {
        ...state,
        ongoingRequest: false,
      };

    case 'Metadata/LOAD_ALLOWED_QUERIES':
      return {
        ...state,
        allowedQueries: action.data,
      };
    case 'Metadata/ADD_ALLOWED_QUERIES':
      return {
        ...state,
        allowedQueries: [...state.allowedQueries, ...action.data],
      };
    case 'Metadata/DELETE_ALLOW_LIST':
      return {
        ...state,
        allowedQueries: [],
      };
    case 'Metadata/DELETE_ALLOWED_QUERY':
      return {
        ...state,
        allowedQueries: [
          ...state.allowedQueries.filter(q => q.name !== action.data),
        ],
      };
    case 'Metadata/UPDATE_ALLOWED_QUERY':
      return {
        ...state,
        allowedQueries: [
          ...state.allowedQueries.map(q =>
            q.name === action.data.queryName ? action.data.newQuery : q
          ),
        ],
      };
    case 'Metadata/ADD_INHERITED_ROLE':
      return {
        ...state,
        inheritedRoles: [...state.inheritedRoles, action.data],
      };
    case 'Metadata/DELETE_INHERITED_ROLE':
      return {
        ...state,
        inheritedRoles: [
          ...state.inheritedRoles.filter(ir => ir.role_name !== action.data),
        ],
      };
    case 'Metadata/UPDATE_INHERITED_ROLE':
      return {
        ...state,
        inheritedRoles: [
          ...state.inheritedRoles.map(ir =>
            ir.role_name === action.data.role_name ? action.data : ir
          ),
        ],
      };
    default:
      return state;
  }
};
