export type EventsState = {
  currentTrigger: string;
  loading: boolean;
  error: {
    [component: string]: any;
  };
};

const state: EventsState = {
  currentTrigger: '',
  loading: true,
  error: {},
};

export default state;
