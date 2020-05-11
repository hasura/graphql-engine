import React from 'react';
import { ConnectedComponent, Connect } from 'react-redux';
import { Table, Schema } from './components/Common/utils/pgUtils';
import { EventsState } from './components/Services/Events/state';
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

export type ReduxAction = {
  type: string;
  payload: Nullable<any>;
  data: Nullable<any>;
};

export type MapReduxToProps = (state: ReduxState, ownProps: any) => any;

export type ComponentReduxConnector = (
  connect: Connect
) => ConnectedComponent<React.FC<any>, any>;

export type GetReduxState = () => ReduxState;
