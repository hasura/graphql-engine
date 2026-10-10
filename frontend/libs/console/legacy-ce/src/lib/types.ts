export type FixMe = any;

export type ApiExplorer = {
  authApiExpanded: string;
  currentTab: number;
  headerFocus: boolean;
  loading: boolean;
  mode: string;
  modalState: Record<string, string>;
  explorerData: Record<string, string>;
  displayedApi: DisplayedApiState;
};

export type DisplayedApiState = {
  details: Record<string, string>;
  id: string;
  request: ApiExplorerRequest;
};

export type ApiExplorerRequest = {
  bodyType: string;
  headers: ApiExplorerHeader[];
  headersInitialised: boolean;
  method: string;
  params: string;
  url: string;
};

export type ApiExplorerHeader = {
  key: string;
  value: string;
  isActive: boolean;
  isNewHeader: boolean;
  isDisabled: boolean;
};

// Router Utils
export type ReplaceRouterState = (route: string) => void;

// HGE common types
export type MetadataRequestPayload<A = any> = {
  type: string;
  version?: number;
  args: A;
};

export type RunSqlType = MetadataRequestPayload<{
  cascade?: boolean;
  read_only?: boolean;
  sql: string;
}>;

export type Entry<O, K extends keyof O> = [K, O[K]];
export type Entries<O> = Entry<O, keyof O>[];

declare global {
  const __DEVELOPMENT__: boolean;
}
export type DeepPartial<T> = {
  [P in keyof T]?: DeepPartial<T[P]>;
};

export type NullableProps<T> = { [K in keyof T]: T[K] | null };

export type DeepNullableProps<T> = {
  [K in keyof T]: DeepNullableProps<T[K]> | null;
};
