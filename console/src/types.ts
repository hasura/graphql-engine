import { MapStateToProps as ReduxMapStateToProps } from 'react-redux';
import { Store } from 'redux';
import { ThunkAction, ThunkDispatch } from 'redux-thunk';
import { RouterAction } from 'react-router-redux';
import { Table, Schema } from './components/Common/utils/pgUtils';
import { EventsState } from './components/Services/Events/state';
import { RAEvents } from './components/Services/Events/types';
import { TelemetryState } from './telemetry/state';

// TODO clean this with approprate type once metadata reducer migrated to TS
import metadataState from './components/Services/Settings/State';
import { addState } from './components/Services/RemoteSchema/state';

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
    latestStableServerVersion: string;
    migrationMode: boolean;
  };
  telemetry: TelemetryState;
  // Todo change this after metadata reducer
  metadata: typeof metadataState;
  remoteSchemas: {
    addData: typeof addState;
  };
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

export type StringObject = {
  [key: string]: string;
};

export type RawHeaderType = {
  name: string;
  value?: string;
  value_from_env?: string;
};
