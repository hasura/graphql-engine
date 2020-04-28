import { Table, Schema } from './components/Common/utils/pgUtils';
import { EventsState } from './components/Services/Events/state';

export type ReduxState = {
  tables: {
    schemaList: Schema[];
    allSchemas: Table[];
  };
  events: EventsState;
};
