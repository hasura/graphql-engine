import React from 'react';
import { ConnectedComponent, Connect } from 'react-redux';
import { Action } from 'redux'
import { ThunkAction, ThunkDispatch } from 'redux-thunk'
import { Table, Schema } from './components/Common/utils/pgUtils';
import { EventsState } from './components/Services/Events/state';
import { RAEvents } from './components/Services/Events/types';
import { Nullable } from './components/Common/utils/tsUtils';

export type ReduxState = {
  tables: {
    schemaList: Schema[];
    allSchemas: Table[];
    dataHeaders: Record<string, string>;
  };
  events: EventsState;
  main: {
    readOnlyMode: boolean;
  };
};

export type ReduxAction = RAEvents | {
  type: string;
  payload: any;
  data: any;
};

export type MapReduxToProps = (state: ReduxState, ownProps: any) => any;

export type ComponentReduxConnector = (
  connect: Connect
) => ConnectedComponent<React.FC<any>, any>;

export type GetReduxState = () => ReduxState;

export type Thunk<ReturnType = void> = ThunkAction<
  ReturnType,
  ReduxState,
  unknown,
  Action<string>
>;

// TODO: proper solution for dispatching "push" actions as they return void
export type Dispatch = (action: (ReduxAction | Thunk | void)) => any;
