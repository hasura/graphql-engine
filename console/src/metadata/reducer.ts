import { MetadataActions } from './actions';
import {
  QueryCollection,
  HasuraMetadataV3,
  RestEndpointEntry,
  InheritedRole,
} from './types';
import { allowedQueriesCollection } from './utils';

type MetadataState = {
  metadataObject: null | HasuraMetadataV3;
  error: null | string | boolean;
  loading: boolean;
  inconsistentObjects: any[];
  ongoingRequest: boolean; // deprecate
  allowedQueries: QueryCollection[];
  inheritedRoles: InheritedRole[];
  rest_endpoints?: RestEndpointEntry[];
};

const defaultState: MetadataState = {
  metadataObject: null,
  error: null,
  loading: false,
  inconsistentObjects: [],
  ongoingRequest: false,
  allowedQueries: [],
  inheritedRoles: [],
};

export const metadataReducer = (
  state = defaultState,
  action: MetadataActions
): MetadataState => {
  switch (action.type) {
    case 'Metadata/EXPORT_METADATA_SUCCESS':
      return {
        ...state,
        metadataObject: action.data,
        allowedQueries:
          action.data?.query_collections?.find(
            query => query.name === allowedQueriesCollection
          )?.definition.queries || [],
        inheritedRoles: action.data?.inherited_roles,
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
        inconsistentObjects: action.data,
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

    case 'Metadata/ADD_REST_ENDPOINT':
      return {
        ...state,
        rest_endpoints: action.data,
      };
    case 'Metadata/DROP_REST_ENDPOINT':
      return {
        ...state,
        rest_endpoints: action.data,
      };
    default:
      return state;
  }
};
