import { Triggers } from './types';

export type EventsState = {
  triggers: Triggers;
  currentTrigger: string;
  loading: {
    [component: string]: boolean;
  };
  error: {
    [component: string]: any;
  };
};

const state: EventsState = {
  triggers: {
    scheduled: [],
    event: [],
  },
  currentTrigger: '',
  loading: {},
  error: {},
};

export default state;
