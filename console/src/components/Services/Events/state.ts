import { Triggers } from './Types';

export type TriggersState = {
  triggers: Triggers;
  currentTrigger: string;
  loading: {
    [component: string]: boolean;
  };
  error: {
    [component: string]: any;
  };
};

const state: TriggersState = {
  triggers: {
    scheduled: [],
    event: [],
  },
  currentTrigger: '',
  loading: {},
  error: {},
};

export default state;
