import eventTriggerReducer from './EventActions';
import addTriggerReducer from './Add/AddActions';
import modifyTriggerReducer from './Modify/Actions';

const eventReducer = {
  triggers: eventTriggerReducer,
  addTrigger: addTriggerReducer,
  modifyTrigger: modifyTriggerReducer,
};

export default eventReducer;
