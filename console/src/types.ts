import { MapStateToProps as ReduxMapStateToProps } from 'react-redux';
import { Store, AnyAction } from 'redux';
import { ThunkAction, ThunkDispatch } from 'redux-thunk';
import { RouterAction } from 'react-router-redux';
import { RAEvents } from './components/Services/Events/types';
import reducer from './reducer';

// Redux Utils
export type ReduxState = ReturnType<typeof reducer>;

export type ReduxAction = RAEvents | RouterAction;
export type MapStateToProps<
  StateProps = unknown,
  OwnProps = unknown
> = ReduxMapStateToProps<StateProps, OwnProps, ReduxState>;
export type GetReduxState = () => ReduxState;
export type Thunk<
  ReturnType = void,
  A extends AnyAction = ReduxAction
> = ThunkAction<ReturnType, ReduxState, unknown, A>;
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
