import { HeaderState } from '../../Common/Layout/ReusableHeader/types';
import { PermissionsType } from './Permissions/types';
import { RemoteSchemaPermissionsState } from './Permissions/state';

type RemoteSchemaHeaders = {
  name: string;
  value?: string;
  value_from_env?: string;
};

type RemoteSchemaDefinition = {
  forward_client_headers: boolean;
  headers: RemoteSchemaHeaders[];
  timeout_seconds: number;
  url: string;
};

export type RemoteSchema = {
  definition: RemoteSchemaDefinition;
  name: string;
  id: number;
  comment: string | null;
  permissions: PermissionsType[];
};

export type AsyncState = {
  isRequesting: boolean;
  isError: any;
  isFetching: boolean;
  isFetchError: any;
};

export type EditState = {
  id: number;
  isModify: boolean;
  originalName: string;
  originalHeaders: any[];
  originalUrl: string;
  originalEnvUrl: string;
  originalTimeoutConf: string;
  originalForwardClientHeaders: boolean;
};

export type AddState = AsyncState & {
  manualUrl: string;
  envName: any;
  headers: any[];
  timeoutConf: string;
  name: string;
  forwardClientHeaders: boolean;
  editState: EditState;
};

export type ListState = AsyncState & {
  remoteSchemas: RemoteSchema[];
  filtered: any[];
  searchQuery: string;
  viewRemoteSchema: string;
};

export type RemoteSchemaState = {
  addData: AddState;
  listData: ListState;
  permissions: RemoteSchemaPermissionsState;
  headerData: HeaderState;
};
