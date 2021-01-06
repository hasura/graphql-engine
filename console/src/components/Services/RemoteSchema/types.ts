import { HeaderState } from '../../Common/Layout/ReusableHeader/types';
import { RemoteSchemaPermissionsState } from './Permissions/types';

export type AsyncState = {
  isRequesting: boolean;
  isError: boolean;
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
  remoteSchemas: any[];
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
