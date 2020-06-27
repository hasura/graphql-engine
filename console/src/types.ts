import { MapStateToProps as ReduxMapStateToProps } from 'react-redux';
import { Store } from 'redux';
import { ThunkAction, ThunkDispatch } from 'redux-thunk';
import { RAEvents } from './components/Services/Events/types';
import { TelemetryActionTypes } from './telemetry/Actions';
import { ProgressBarAction } from './components/App/Actions';
import reducer from './reducer';

// Redux Utils
export type ReduxState = ReturnType<typeof reducer>;

export type ReduxAction = RAEvents | TelemetryActionTypes | ProgressBarAction;

export type MapStateToProps<
  StateProps = unknown,
  OwnProps = unknown
> = ReduxMapStateToProps<StateProps, OwnProps, ReduxState>; // TODO: remove

export type GetReduxState = () => ReduxState; // TODO: remove

export type Thunk<ReturnType = void> = ThunkAction<
  ReturnType,
  ReduxState,
  unknown,
  ReduxAction
>;

export type Dispatch = ThunkDispatch<ReduxState, unknown, ReduxAction>; // TODO: remove

export type ConnectInjectedProps = {
  dispatch: Dispatch;
}; // TODO: remove

export type ReduxStore = Store<ReduxState, ReduxAction>;

// Router Utils
export type ReplaceRouterState = (route: string) => void;
