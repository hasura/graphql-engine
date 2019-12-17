import eventTriggerReducer from './EventActions';
import addTriggerReducer from './Add/AddActions';
import modifyTriggerReducer from './Modify/Actions';
import invokeEventTriggerReducer from './Common/InvokeManualTrigger/InvokeManualTriggerAction';
import scheduledTriggerReducer from './ScheduledTriggers/Actions';

const eventReducer = {
  triggers: eventTriggerReducer,
  addTrigger: addTriggerReducer,
  modifyTrigger: modifyTriggerReducer,
  invokeEventTrigger: invokeEventTriggerReducer,
  scheduledTrigger: scheduledTriggerReducer,
};

export default eventReducer;
