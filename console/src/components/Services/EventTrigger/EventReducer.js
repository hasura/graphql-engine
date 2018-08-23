import eventTriggerReducer from './EventActions';
import addTriggerReducer from './Add/AddActions';

const eventReducer = {
  triggers: eventTriggerReducer,
  addTrigger: addTriggerReducer,
};

export default eventReducer;
