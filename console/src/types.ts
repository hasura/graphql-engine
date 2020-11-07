import { MapStateToProps as ReduxMapStateToProps } from 'react-redux';
import { Store } from 'redux';
import { ThunkAction, ThunkDispatch } from 'redux-thunk';
import { RouterAction } from 'react-router-redux';
import { Table, Schema } from './components/Common/utils/pgUtils';
import { EventsState } from './components/Services/Events/state';
import { RAEvents } from './components/Services/Events/types';
import { ConsoleNotification } from './components/Main/ConsoleNotification';
import { Nullable } from './components/Common/utils/tsUtils';

export type UserTypes = 'admin' | string;

export type NotificationsState = {
  read: 'all' | 'default' | 'error' | string[];
  date: string | null; // ISO String
  showBadge: boolean;
};

export type TelemetryNotificationsState = Record<UserTypes, NotificationsState>;

export type ConsoleState = {
  console_opts: Nullable<{
    telemetryNotificationShown?: boolean;
    disablePreReleaseUpdateNotifications?: boolean;
    console_notifications?: TelemetryNotificationsState;
  }>;
  hasura_uuid: string;
};

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

// Redux Utils
export type ReduxState = {
  tables: {
    schemaList: Schema[];
    allSchemas: Table[];
    dataHeaders: Record<string, string>;
  };
  events: EventsState;
  main: {
    readOnlyMode: boolean;
    serverVersion: string;
    latestPreReleaseServerVersion: string;
    latestStableServerVersion: string;
    consoleNotifications: ConsoleNotification[];
  };
  telemetry: ConsoleState;
  apiexplorer: ApiExplorer;
};

export type ReduxAction = RAEvents | RouterAction;
export type MapStateToProps<
  StateProps = unknown,
  OwnProps = unknown
> = ReduxMapStateToProps<StateProps, OwnProps, ReduxState>;
export type GetReduxState = () => ReduxState;
export type Thunk<ReturnType = void> = ThunkAction<
  ReturnType,
  ReduxState,
  unknown,
  ReduxAction
>;
export type Dispatch = ThunkDispatch<ReduxState, unknown, ReduxAction>;
export type ConnectInjectedProps = {
  dispatch: Dispatch;
};
export type ReduxStore = Store<ReduxState, ReduxAction>;

// Router Utils
export type ReplaceRouterState = (route: string) => void;

// HGE common types
export type RunSqlType = {
  type: string;
  version?: number;
  args: {
    cascade?: boolean;
    read_only?: boolean;
    sql: string;
  };
};
