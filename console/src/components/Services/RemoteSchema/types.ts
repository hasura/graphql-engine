import { RemoteSchemaPermissionsState } from './Permissions/state';
import { RemoteSchema } from '../../../metadata/types';

type HeaderState = {
  headers: [
    {
      name: string;
      type: string;
      value: string;
    }
  ];
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
  originalComment?: string;
};

export type AddState = AsyncState & {
  manualUrl: string;
  envName: any;
  headers: any[];
  timeoutConf: string;
  name: string;
  forwardClientHeaders: boolean;
  editState: EditState;
  comment?: string;
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
