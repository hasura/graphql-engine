import { MapStateToProps as ReduxMapStateToProps } from 'react-redux';
import { Store } from 'redux';
import { ThunkAction, ThunkDispatch } from 'redux-thunk';
import { RouterAction } from 'react-router-redux';
import { Table, Schema } from './components/Common/utils/pgUtils';
import { EventsState } from './components/Services/Events/state';
import { RAEvents } from './components/Services/Events/types';
import { TelemetryState } from './telemetry/state';
import { MetadataObject } from './components/Services/Settings/utils';

// Redux Utils
export type ReduxState = {
  actions: {
    common: {
      actions: MetadataObject[];
    };
  };
  tables: {
    schemaList: Schema[];
    allSchemas: Table[] | MetadataObject[];
    trackedFunctions: MetadataObject[];
    dataHeaders: Record<string, string>;
  };
  events: EventsState;
  main: {
    readOnlyMode: boolean;
    serverVersion: string;
    latestStableServerVersion: string;
  };
  telemetry: TelemetryState;
  remoteSchemas: {
    listData: {
      remoteSchemas: MetadataObject[];
    };
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
